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
moduleDriver, interpretModule, newModuleDriver
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

import qualified Core.ExprAST as E
import qualified Core.ModuleAST as M
import Core.ModuleAST (debugModuleExpr)
import Core.Reorderer (reorder)
import Core.Renamer (rename)
import Core.Validator (validate)
import Core.TypeChecker --(typeCheck, TypeVarEnv, TypeCons)
import Utils.Utils
import qualified Utils.OrdMap as OrdMap


-- | moduleDriver takes a list of base modules and creates a runtime module envinroment that is used create close modules
-- for simulation
moduleDriver :: [M.TopMod E.SrcId] -> IO (Maybe M.ModuleEnv)
moduleDriver baseModules = do
    -- a rudimentary controlelr that sets up the iteration over the luist of module commands, altering state as we fold
    modEnv <- DF.foldlM procMod Map.empty baseModules
    infoM "ode3.moduleDriver" "No fatal errors"
    infoM "ode3.moduleDriver" ("Final module environment - " ++ (show $ Map.keys modEnv))
    return $ Just modEnv
  where
    -- processes a module and displays the results
    procMod :: M.ModuleEnv -> M.TopMod E.SrcId -> IO M.ModuleEnv
    procMod modEnv (M.TopMod name mod) = case interpretRes of
        Left err -> errorOut err >> return modEnv
        Right mod -> succOut mod >> return (Map.insert name mod modEnv)
      where
        interpretRes = checkName *> interpretModule modEnv mod
        -- check if module already exists
        checkName = if Map.member name modEnv then throwError ("(MD06) - Module with name " ++ (show name) ++ " already defined") else pure ()
        errorOut err = errorM "ode3.moduleDriver" ("Error processing module " ++ name ++ " " ++ err)
        succOut mod = infoM "ode3.moduleDriver" ("Processed module " ++ name ++ " - " ++ prettyPrint mod) >>
            debugM "ode3.moduleDriver" ("Module toplevel - \n" ++ debugModuleExpr mod)



newModuleDriver :: M.ModuleEnv -> M.TopMod E.SrcId -> MExcept M.ModuleEnv
newModuleDriver modEnv topMod@(M.TopMod name mod) =
    Map.insert <$> pure name <*> eRes <*> pure modEnv
  where
    eRes = checkName *> interpretModule modEnv mod
    -- check if module already exists
    checkName = if Map.member name modEnv then throwError ("(MD06) - Module with name " ++ (show name) ++ " already defined") else pure ()


-- a basic interpreter over the set of module types, interpres the modules with regards tro the moduleenv
interpretModule :: M.ModuleEnv -> M.Module E.SrcId -> MExcept (M.Module E.Id)
interpretModule modEnv mod@(M.LitMod _ _) = do
    -- reorder, rename and typecheck the expressinons within module, adding to the module metadata
    mod' <- validate >=> reorder >=> rename >=> typeCheck $ mod
    return mod'

interpretModule modEnv mod@(M.FunctorMod _ _ _) = do
    -- reorder, rename and typecheck the expressinons within functor module, adding to the module metadata
    mod' <- validate >=> reorder >=> rename >=> typeCheck $ mod
    return mod'

-- simply lookup the id within the env and return the module
interpretModule modEnv mod@(M.VarMod modId) = case (Map.lookup modId modEnv) of
    Just mod -> return mod
    Nothing -> throwError $ "(MD07) - Referenced module " ++ (show modId) ++ " not found in envirnoment"

interpretModule modEnv mod@(M.AppMod fModId argMods) = do

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
    eFMod :: MExcept (M.Module E.Id)
    eFMod = case (Map.lookup fModId modEnv) of
        Just mod -> case mod of
            (M.FunctorMod _ _ _) -> return mod
            _ -> throwError ("(MD01) - Module " ++ fModId ++ " is not a functor")
        Nothing -> throwError ("(MD02) - Functor module " ++ fModId ++ " not found")


    -- TODO - need typecheck
    -- interpret the args, either eval inline apps or lookup
    -- map and sequence thru interpretation of the args, using the same env
    eArgMods :: MExcept [M.Module E.Id]
    eArgMods = DT.mapM interpretArgs argMods
      where
        interpretArgs argMod = do
            mod <- interpretModule modEnv argMod
            case mod of
                (M.LitMod _ _) -> return mod
                _ -> throwError ("(MD03) - Module argument to functor " ++ fModId ++ " is not a literal module")

    -- rename the argMods according to pos/ create a new modenv to evalulate the applciation within
    getAppModEnv :: M.Module E.Id -> [M.Module E.Id] -> MExcept M.ModuleEnv
    getAppModEnv (M.FunctorMod funArgs _ _) argMods = if (OrdMap.size funArgs == length argMods)
        then return (Map.fromList $ zip (OrdMap.keys funArgs) argMods)
        else throwError "(MD05) - Wrong number of arguments for functor application"

-- actually evaluate the functor applciation, similar to evaluation of function application
applyFunctor :: M.Module E.Id -> M.ModuleEnv -> M.Module E.Id
applyFunctor fMod@(M.FunctorMod fArgs fExprMap fModData) modEnv = resMod
  where
    -- best way to do this, create an empty lit mod, and add to it all the data by folding over the modEnv
    baseMod = M.LitMod fExprMap fModData
    resMod = Map.foldrWithKey appendModule baseMod modEnv

appendModule :: E.SrcId -> M.Module E.Id -> M.Module E.Id -> M.Module E.Id
appendModule argName argMod@(M.LitMod argExprMap argModData) baseMod@(M.LitMod baseExprMap baseModData) = baseMod'
  where
    baseMod' = M.LitMod exprMap' modData'
    deltaId = fromJust $ (M.modFreeId argModData)
    modData' = appendModuleData argModData baseModData

    -- update the expressions ids
    exprMap' = OrdMap.union argExprMap (OrdMap.map updateExprs baseExprMap)
    updateExprs (topB, topExpr) = (fmap (+ deltaId) topB, topExpr'')
      where
        topExpr' = fmap (+ deltaId) topExpr
        topExpr'' = case topExpr' of
                    (E.TopLet b expr) -> E.TopLet b (updateVars expr)
                    (E.TopAbs b arg expr) -> E.TopAbs b arg (updateVars expr)

        -- change all refernces from ModVar to LocalVar using the new ids, use the arg Id Bimap to calc
        argIdBimap = M.modIdBimap argModData

        updateVars :: E.Expr E.Id -> E.Expr E.Id
        updateVars (E.Var (E.ModVar modId id))
            | modId == argName = E.Var (E.LocalVar (argIdBimap Bimap.! id))

        updateVars (E.App vId@(E.ModVar modId id) expr)
            | modId == argName = E.App (E.LocalVar (argIdBimap Bimap.! id)) (updateVars expr)
        updateVars (E.App vId expr) = E.App vId (updateVars expr)

        updateVars (E.Let b e1 e2) = E.Let b (updateVars e1) (updateVars e2)
        updateVars (E.Op op e) = E.Op op (updateVars e)
        updateVars (E.If eIf eT eF) = E.If (updateVars eIf) (updateVars eT) (updateVars eF)
        updateVars (E.Tuple es) = E.Tuple $ map updateVars es
        updateVars expr = expr


-- update mod data - sigMap doesn't change
appendModuleData :: M.ModuleData -> M.ModuleData -> M.ModuleData
appendModuleData argModData baseModData = baseModData { M.modTMap = typeMap', M.modIdBimap = idBimap', M.modFreeId = freeId' }
  where
    deltaId = fromJust $ (M.modFreeId argModData)
    typeMap' = Map.union (M.modTMap argModData) (Map.mapKeys (+ deltaId) (M.modTMap baseModData)) -- update the base ids and merge
    idBimap' = Bimap.fromList . map (\(srcId, id) -> (srcId, id + deltaId)) . Bimap.toList . M.modIdBimap $ baseModData   -- update the base ids only
    freeId' = liftM2 (+) (M.modFreeId argModData) (M.modFreeId baseModData) -- sum of both numbers



















