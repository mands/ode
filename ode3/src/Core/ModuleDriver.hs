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

module Core.ModuleDriver (
moduleDriver,
) where


import Debug.Trace
import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Bimap as Bimap
import Data.Maybe (fromJust)
import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Error

import System.Log.Logger

import qualified Core.AST as C
import Core.AST.Module (debugModuleExpr)
import Core.Reorderer (reorder)
import Core.Renamer (rename)
import Core.Validator (validate)
import Core.TypeChecker --(typeCheck, TypeVarEnv, TypeCons)
import Utils.Utils
import qualified Utils.OrdMap as OrdMap


-- | moduleDriver takes a list of base modules and creates a runtime module envinroment that is used create close modules
-- for simulation
moduleDriver :: [C.TopMod C.SrcId] -> IO (Maybe C.ModuleEnv)
moduleDriver baseModules = do
    -- a rudimentary controlelr that sets up the iteration over the luist of module commands, altering state as we fold
    modEnv <- DF.foldlM procMod Map.empty baseModules
    infoM "ode3.moduleDriver" "No fatal errors"
    infoM "ode3.moduleDriver" ("Final module environment - " ++ (show $ Map.keys modEnv))
    return $ Just modEnv
  where
    -- processes a module and displays the results
    procMod :: C.ModuleEnv -> C.TopMod C.SrcId -> IO C.ModuleEnv
    procMod modEnv (C.TopMod name mod) = either
        (\err -> errorOut err >> return modEnv)
        (\mod -> succOut mod >> return (Map.insert name mod modEnv)) interpretRes
      where
        interpretRes = checkName *> interpretModule modEnv mod
        -- check if module already exists
        checkName = if Map.member name modEnv then throwError ("(MD06) - Module with name " ++ (show name) ++ " already defined") else pure ()
        errorOut err = errorM "ode3.moduleDriver" ("Error processing module " ++ name ++ " " ++ err)
        succOut mod = infoM "ode3.moduleDriver" ("Processed module " ++ name ++ " - " ++ prettyPrint mod) >>
            debugM "ode3.moduleDriver" ("Module toplevel - \n" ++ debugModuleExpr mod)


-- a basic interpreter over the set of module types, interpres the modules with regards tro the moduleenv
interpretModule :: C.ModuleEnv -> C.Module C.SrcId -> MExcept (C.Module C.Id)
interpretModule modEnv mod@(C.LitMod _ _) = do
    -- reorder, rename and typecheck the expressinons within module, adding to the module metadata
    mod' <- validate >=> reorder >=> rename >=> typeCheck $ mod
    return mod'

interpretModule modEnv mod@(C.FunctorMod _ _ _) = do
    -- reorder, rename and typecheck the expressinons within functor module, adding to the module metadata
    mod' <- validate >=> reorder >=> rename >=> typeCheck $ mod
    return mod'

-- simply lookup the id within the env and return the module
interpretModule modEnv mod@(C.VarMod modId) = case (Map.lookup modId modEnv) of
    Just mod -> return mod
    Nothing -> throwError $ "(MD07) - Referenced module " ++ (show modId) ++ " not found in envirnoment"

interpretModule modEnv mod@(C.AppMod fModId argMods) = do

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
    eFMod :: MExcept (C.Module C.Id)
    eFMod = case (Map.lookup fModId modEnv) of
        Just mod -> case mod of
            (C.FunctorMod _ _ _) -> return mod
            _ -> throwError ("(MD01) - Module " ++ fModId ++ " is not a functor")
        Nothing -> throwError ("(MD02) - Functor module " ++ fModId ++ " not found")


    -- TODO - need typecheck
    -- interpret the args, either eval inline apps or lookup
    -- map and sequence thru interpretation of the args, using the same env
    eArgMods :: MExcept [C.Module C.Id]
    eArgMods = DT.mapM interpretArgs argMods
      where
        interpretArgs argMod = do
            mod <- interpretModule modEnv argMod
            case mod of
                (C.LitMod _ _) -> return mod
                _ -> throwError ("(MD03) - Module argument to functor " ++ fModId ++ " is not a literal module")

    -- rename the argMods according to pos/ create a new modenv to evalulate the applciation within
    getAppModEnv :: C.Module C.Id -> [C.Module C.Id] -> MExcept C.ModuleEnv
    getAppModEnv (C.FunctorMod funArgs _ _) argMods = if (OrdMap.size funArgs == length argMods)
        then return (Map.fromList $ zip (OrdMap.keys funArgs) argMods)
        else throwError "(MD05) - Wrong number of arguments for functor application"

-- actually evaluate the functor applciation, similar to evaluation of function application
applyFunctor :: C.Module C.Id -> C.ModuleEnv -> C.Module C.Id
applyFunctor fMod@(C.FunctorMod fArgs fExprMap fModData) modEnv = resMod
  where
    -- best way to do this, create an empty lit mod, and add to it all the data by folding over the modEnv
    baseMod = C.LitMod fExprMap fModData
    resMod = Map.foldrWithKey appendModule baseMod modEnv

appendModule :: C.SrcId -> C.Module C.Id -> C.Module C.Id -> C.Module C.Id
appendModule argName argMod@(C.LitMod argExprMap argModData) baseMod@(C.LitMod baseExprMap baseModData) = baseMod'
  where
    baseMod' = C.LitMod exprMap' modData'
    deltaId = fromJust $ (C.modFreeId argModData)
    modData' = appendModuleData argModData baseModData

    -- update the expressions ids
    exprMap' = OrdMap.union argExprMap (OrdMap.map updateExprs baseExprMap)
    updateExprs (topB, topExpr) = (fmap (+ deltaId) topB, topExpr'')
      where
        topExpr' = fmap (+ deltaId) topExpr
        topExpr'' = case topExpr' of
                    (C.TopLet b expr) -> C.TopLet b (updateVars expr)
                    (C.TopAbs b arg expr) -> C.TopAbs b arg (updateVars expr)

        -- change all refernces from ModVar to LocalVar using the new ids, use the arg Id Bimap to calc
        argIdBimap = C.modIdBimap argModData

        updateVars :: C.Expr C.Id -> C.Expr C.Id
        updateVars (C.Var (C.ModVar modId id))
            | modId == argName = C.Var (C.LocalVar (argIdBimap Bimap.! id))

        updateVars (C.App vId@(C.ModVar modId id) expr)
            | modId == argName = C.App (C.LocalVar (argIdBimap Bimap.! id)) (updateVars expr)
        updateVars (C.App vId expr) = C.App vId (updateVars expr)

        updateVars (C.Let b e1 e2) = C.Let b (updateVars e1) (updateVars e2)
        updateVars (C.Op op e) = C.Op op (updateVars e)
        updateVars (C.If eIf eT eF) = C.If (updateVars eIf) (updateVars eT) (updateVars eF)
        updateVars (C.Tuple es) = C.Tuple $ map updateVars es
        updateVars expr = expr


-- update mod data - sigMap doesn't change
appendModuleData :: C.ModuleData -> C.ModuleData -> C.ModuleData
appendModuleData argModData baseModData = baseModData { C.modTMap = typeMap', C.modIdBimap = idBimap', C.modFreeId = freeId' }
  where
    deltaId = fromJust $ (C.modFreeId argModData)
    typeMap' = Map.union (C.modTMap argModData) (Map.mapKeys (+ deltaId) (C.modTMap baseModData)) -- update the base ids and merge
    idBimap' = Bimap.fromList . map (\(srcId, id) -> (srcId, id + deltaId)) . Bimap.toList . C.modIdBimap $ baseModData   -- update the base ids only
    freeId' = liftM2 (+) (C.modFreeId argModData) (C.modFreeId baseModData) -- sum of both numbers



















