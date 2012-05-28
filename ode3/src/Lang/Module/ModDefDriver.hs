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

import System.Log.Logger

import Control.Applicative
import Control.Monad
import Control.Monad.Error

import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.List.Split as ListSplit
import qualified Data.Bimap as Bimap
import Data.Maybe (isJust, fromJust)
import Text.Printf (printf)

import qualified Utils.OrdMap as OrdMap
import qualified Utils.OrdSet as OrdSet
import Utils.Utils
import UI.SysState

import Lang.Common.AST
import Lang.Module.AST
import Lang.Core.AST

--import Core.Reorderer (reorder)
import Lang.Core.Renamer (rename)
import Lang.Core.Validator (validate)
import Lang.Core.TypeChecker


-- Evaluate Module Defintions ------------------------------------------------------------------------------------------

-- a basic interpreter over the set of module types, interpres the modules with regards to the moduleenv
evalModDef :: GlobalModEnv -> FileData -> Module DesId -> MExcept (Module Id)
evalModDef gModEnv fileData mod@(LitMod _ _) = do
    -- reorder, rename and typecheck the expressinons within module, adding to the module metadata
    -- mod' <- validate >=> reorder >=> rename >=> typeCheck $ mod

    -- TODO - these require gModEnv for in-module import lookups
    (gModEnv', mod') <- validate >=> rename >=> typeCheck $ (gModEnv, mod)
    return mod'

evalModDef gModEnv fileData mod@(FunctorMod _ _ _) = do
    -- reorder, rename and typecheck the expressinons within functor module, adding to the module metadata
    -- mod' <- validate >=> reorder >=> rename >=> typeCheck $ mod
    (gModEnv', mod') <- validate >=> rename >=> typeCheck $ (gModEnv, mod)
    return mod'

-- simply looks up the id within both the file and then global env and return the module if found
evalModDef gModEnv fileData mod@(VarMod modName) = do
    case (Map.lookup modName fModEnv) of -- look locally within fModEnv first
        Just mod    -> return mod
        Nothing     -> eModFullName >>= (\modFullName -> getGlobalMod modFullName gModEnv) -- if not local, check imnports and gModEnv

  where
    fModEnv = fileModEnv fileData
    fImportMap = fileImportMap fileData
    eModFullName = maybeToExcept (Map.lookup modName fImportMap) $ printf "Module %s not imported within file" (show modName)


-- TODO - need to update to not actually apply the functor, just link and update the module sigMap
evalModDef gModEnv fileData mod@(AppMod fModId argMods) = do
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
    -- let mod' = applyFunctor fMod' appModEnv'
    -- return $ mod'
    return fMod'
  where
    modEnv = undefined -- TODO update
    -- lookup/evaluate the functor and params, dynamically type-check
    eFMod :: MExcept (Module Id)
    eFMod = case (Map.lookup fModId modEnv) of
        Just mod -> case mod of
            (FunctorMod _ _ _) -> return mod
            _ -> throwError ("(MD01) - Module " ++ show fModId ++ " is not a functor")
        Nothing -> throwError ("(MD02) - Functor module " ++ show fModId ++ " not found")

    -- interpret the args, either eval inline apps or lookup
    -- map and sequence thru interpretation of the args, using the same env
    eArgMods :: MExcept [Module Id]
    eArgMods = DT.mapM interpretArgs argMods
      where
        interpretArgs argMod = do
            mod <- evalModDef gModEnv fileData argMod
            case mod of
                (LitMod _ _) -> return mod
                _ -> throwError ("(MD03) - Module argument to functor " ++ show fModId ++ " is not a literal module")

    -- rename the argMods according to pos/ create a new modenv to evalulate the applciation within
    getAppModEnv :: Module Id -> [Module Id] -> MExcept FileModEnv
    getAppModEnv (FunctorMod funArgs _ _) argMods = if (OrdMap.size funArgs == length argMods)
        then return (Map.fromList $ zip (OrdMap.keys funArgs) argMods)
        else throwError "(MD05) - Wrong number of arguments for functor application"


-- Functor Application Helper Funcs ------------------------------------------------------------------------------------
-- TODO - utilise this code later when we unfold all modules into a single block of code

-- actually evaluate the functor applciation, similar to evaluation of function application
applyFunctor :: Module Id -> FileModEnv -> Module Id
applyFunctor fMod@(FunctorMod fArgs fExprMap fModData) modEnv = resMod
  where
    -- best way to do this, create an empty lit mod, and add to it all the data by folding over the modEnv
    baseMod = LitMod fExprMap fModData
    resMod = Map.foldrWithKey appendModule baseMod modEnv

-- adds one module into another, basically mappend
appendModule :: ModName -> Module Id -> Module Id -> Module Id
appendModule argName argMod@(LitMod argExprMap argModData) baseMod@(LitMod baseExprMap baseModData) = baseMod'
  where
    baseMod' = LitMod exprMap' modData'
    deltaId = fromJust $ (modFreeId argModData)
    modData' = appendModuleData argModData baseModData

    -- update the expressions ids
    exprMap' = OrdMap.union argExprMap (OrdMap.map updateExprs baseExprMap)
    updateExprs (topB, topExpr) = (fmap (+ deltaId) topB, topExpr'')
      where
        topExpr'@(TopLet s b expr) = fmap (+ deltaId) topExpr
        topExpr'' = TopLet s b (updateVars expr)

        -- change all refernces from ModVar to LocalVar using the new ids, use the arg Id Bimap to calc
        -- TODO - use a traversable?
        argIdBimap = modIdBimap argModData

        updateVars :: Expr Id -> Expr Id
        updateVars (Var (ModVar modId id))
            | modId == argName = Var (LocalVar (argIdBimap Bimap.! id))

        updateVars (App vId@(ModVar modId id) expr)
            | modId == argName = App (LocalVar (argIdBimap Bimap.! id)) (updateVars expr)
        updateVars (App vId expr) = App vId (updateVars expr)

        updateVars (Abs v expr) = Abs v (updateVars expr)
        updateVars (Let s b e1 e2) = Let s b (updateVars e1) (updateVars e2)
        updateVars (Op op e) = Op op (updateVars e)
        updateVars (If eIf eT eF) = If (updateVars eIf) (updateVars eT) (updateVars eF)
        updateVars (Tuple es) = Tuple $ map updateVars es
        updateVars expr = expr


-- update mod data - sigMap doesn't change
appendModuleData :: ModData -> ModData -> ModData
appendModuleData argModData baseModData = baseModData { modTMap = typeMap', modIdBimap = idBimap', modFreeId = freeId' }
  where
    deltaId = fromJust $ (modFreeId argModData)
    typeMap' = Map.union (modTMap argModData) (Map.mapKeys (+ deltaId) (modTMap baseModData)) -- update the base ids and merge
    idBimap' = Bimap.fromList . map (\(srcId, id) -> (srcId, id + deltaId)) . Bimap.toList . modIdBimap $ baseModData   -- update the base ids only
    freeId' = liftM2 (+) (modFreeId argModData) (modFreeId baseModData) -- sum of both numbers


