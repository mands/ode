-----------------------------------------------------------------------------
--
-- Module      :  Core.ModuleDriver
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Runs the module system and interprets all module level commands
-- driver for the core language front-end of the compiler
-- will effectively run the front-end pipeline within the Error monad
-- requires calling reorderer, renamer, typechecker, converter/interpreter
-----------------------------------------------------------------------------

module Lang.Module.ModDefDriver (
evalModDef
) where

import Debug.Trace
import System.Log.Logger

import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Error
import qualified Control.Conditional as Cond

import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.List.Split as ListSplit
import qualified Data.Bimap as Bimap
import Data.Maybe (isJust, fromJust)

import qualified System.Directory as Dir
import qualified System.FilePath as FP

import Lang.Module.Parser
import qualified Lang.Common.Parser as CP
import Lang.Common.AST

import qualified Utils.OrdMap as OrdMap
import qualified Utils.OrdSet as OrdSet

import qualified Lang.Core.AST as E
import Lang.Module.AST
--import Core.Reorderer (reorder)
import Lang.Core.Renamer (rename)
import Lang.Core.Validator (validate)
import Lang.Core.TypeChecker --(typeCheck, TypeVarEnv, TypeCons)
import Utils.Utils
import UI.ShellState



-- a basic interpreter over the set of module types, interpres the modules with regards tro the moduleenv
evalModDef :: LocalModEnv -> Module E.DesId -> MExcept (Module E.Id)
evalModDef modEnv mod@(LitMod _ _) = do
    -- reorder, rename and typecheck the expressinons within module, adding to the module metadata
    -- mod' <- validate >=> reorder >=> rename >=> typeCheck $ mod
    mod' <- validate >=> rename >=> typeCheck $ mod
    return mod'

evalModDef modEnv mod@(FunctorMod _ _ _) = do
    -- reorder, rename and typecheck the expressinons within functor module, adding to the module metadata
    -- mod' <- validate >=> reorder >=> rename >=> typeCheck $ mod
    mod' <- validate >=> rename >=> typeCheck $ mod
    return mod'

-- TODO - this should look in the global modEnv too
-- simply lookup the id within the env and return the module
evalModDef modEnv mod@(VarMod modId) = case (Map.lookup modName modEnv) of
    Just mod -> return mod
    Nothing -> throwError $ "(MD07) - Referenced module " ++ (show modId) ++ " not found in envirnoment"
  where
    (_, modName) = splitModFullName modId

evalModDef modEnv mod@(AppMod fModId argMods) = do
    -- need to check that the application is valid, if so then create a new module
    -- involves several steps with specialised pipeline operations

    -- reorder - place the expressions from args ahead of thos withing the func module
    -- renaming, use the free vars to deteermine a safe renaimg scheme
    -- typecheck, check the args are valid, then typecheck the signatures, using the same alogirthm as typechecking an app
    --  within an expression, run the same cosntain algorithm, then matchup the sigs

    -- order is, args/sig check, typecheck, rename, reorder
    -- should return a new closed module that can be reused later on
    fMod <- eFMod
    appModEnv <- (getAppModEnv fMod) =<< eArgMods
    (fMod', appModEnv') <- typeCheckApp fMod appModEnv
    let mod' = applyFunctor fMod' appModEnv'
    return $ mod'
  where

    -- lookup/evaluate the functor and params, dynamically type-check
    eFMod :: MExcept (Module E.Id)
    eFMod = case (Map.lookup fModId modEnv) of
        Just mod -> case mod of
            (FunctorMod _ _ _) -> return mod
            _ -> throwError ("(MD01) - Module " ++ show fModId ++ " is not a functor")
        Nothing -> throwError ("(MD02) - Functor module " ++ show fModId ++ " not found")

    -- interpret the args, either eval inline apps or lookup
    -- map and sequence thru interpretation of the args, using the same env
    eArgMods :: MExcept [Module E.Id]
    eArgMods = DT.mapM interpretArgs argMods
      where
        interpretArgs argMod = do
            mod <- evalModDef modEnv argMod
            case mod of
                (LitMod _ _) -> return mod
                _ -> throwError ("(MD03) - Module argument to functor " ++ show fModId ++ " is not a literal module")

    -- rename the argMods according to pos/ create a new modenv to evalulate the applciation within
    getAppModEnv :: Module E.Id -> [Module E.Id] -> MExcept LocalModEnv
    getAppModEnv (FunctorMod funArgs _ _) argMods = if (OrdMap.size funArgs == length argMods)
        then return (Map.fromList $ zip (OrdMap.keys funArgs) argMods)
        else throwError "(MD05) - Wrong number of arguments for functor application"

-- actually evaluate the functor applciation, similar to evaluation of function application
applyFunctor :: Module E.Id -> LocalModEnv -> Module E.Id
applyFunctor fMod@(FunctorMod fArgs fExprMap fModData) modEnv = resMod
  where
    -- best way to do this, create an empty lit mod, and add to it all the data by folding over the modEnv
    baseMod = LitMod fExprMap fModData
    resMod = Map.foldrWithKey appendModule baseMod modEnv

-- adds one module into another, basically mappend
appendModule :: ModName -> Module E.Id -> Module E.Id -> Module E.Id
appendModule argName argMod@(LitMod argExprMap argModData) baseMod@(LitMod baseExprMap baseModData) = baseMod'
  where
    baseMod' = LitMod exprMap' modData'
    deltaId = fromJust $ (modFreeId argModData)
    modData' = appendModuleData argModData baseModData

    -- update the expressions ids
    exprMap' = OrdMap.union argExprMap (OrdMap.map updateExprs baseExprMap)
    updateExprs (topB, topExpr) = (fmap (+ deltaId) topB, topExpr'')
      where
        topExpr'@(E.TopLet s b expr) = fmap (+ deltaId) topExpr
        topExpr'' = E.TopLet s b (updateVars expr)

        -- change all refernces from ModVar to LocalVar using the new ids, use the arg Id Bimap to calc
        -- TODO - use a traversable?
        argIdBimap = modIdBimap argModData

        updateVars :: E.Expr E.Id -> E.Expr E.Id
        updateVars (E.Var (E.ModVar modId id))
            | modId == argName = E.Var (E.LocalVar (argIdBimap Bimap.! id))

        updateVars (E.App vId@(E.ModVar modId id) expr)
            | modId == argName = E.App (E.LocalVar (argIdBimap Bimap.! id)) (updateVars expr)
        updateVars (E.App vId expr) = E.App vId (updateVars expr)

        updateVars (E.Abs v expr) = E.Abs v (updateVars expr)
        updateVars (E.Let s b e1 e2) = E.Let s b (updateVars e1) (updateVars e2)
        updateVars (E.Op op e) = E.Op op (updateVars e)
        updateVars (E.If eIf eT eF) = E.If (updateVars eIf) (updateVars eT) (updateVars eF)
        updateVars (E.Tuple es) = E.Tuple $ map updateVars es
        updateVars expr = expr


-- update mod data - sigMap doesn't change
appendModuleData :: ModData -> ModData -> ModData
appendModuleData argModData baseModData = baseModData { modTMap = typeMap', modIdBimap = idBimap', modFreeId = freeId' }
  where
    deltaId = fromJust $ (modFreeId argModData)
    typeMap' = Map.union (modTMap argModData) (Map.mapKeys (+ deltaId) (modTMap baseModData)) -- update the base ids and merge
    idBimap' = Bimap.fromList . map (\(srcId, id) -> (srcId, id + deltaId)) . Bimap.toList . modIdBimap $ baseModData   -- update the base ids only
    freeId' = liftM2 (+) (modFreeId argModData) (modFreeId baseModData) -- sum of both numbers


