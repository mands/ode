-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.ModDriver.ModDef
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

{-# LANGUAGE FlexibleContexts #-}

module Subsystem.ModDriver.ModDef (
evalModDef, mkRefMod
) where

-- higher-level control
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import qualified Control.Monad.State as S

-- fclabels stuff
import Control.Category
import Data.Label
import Prelude hiding ((.), id)

-- containers
import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.List.Split as ListSplit
import qualified Data.Bimap as Bimap
import Data.Maybe (isJust, fromJust)

-- other
import Text.Printf (printf)
import System.Log.Logger

-- Ode
import Utils.CommonImports
import qualified Utils.OrdMap as OrdMap
import qualified Utils.OrdSet as OrdSet
import qualified Subsystem.SysState as St

import AST.Common
import AST.Module
import AST.Core
import qualified Subsystem.Units as U
import {-# SOURCE #-} Subsystem.ModDriver.ModCmd (evalImport) -- special import to break import cycle

--import Core.Reorderer (reorder)
import Process.Renamer (rename)
import Process.Validator (validate)
import Process.TypeChecker (typeCheck)

-- Evaluate Module Defintions ------------------------------------------------------------------------------------------

-- | Frontend function that pre-processes common functionalty then evaluates the module definition
evalModDef :: FileData -> Module DesId -> St.SysExceptIO (Module Id)
evalModDef fd mod = do
    -- process the imports and extract the units
    mod' <- processModImports mod >>= processModUnits

    -- actually eval the module
    modEnv <- St.getSysState St.vModEnv
    unitsState <- St.getSysState St.lUnitsState
    St.liftExSys $ evalModDef' modEnv fd unitsState mod'
  where
    -- use [importCmds] to process imports for the module and create an import map, can then validate/typecheck/etc. against it
    processModImports :: Module DesId -> St.SysExceptIO (Module DesId)
    processModImports mod =
        maybe (return mod) (\modData -> putModData mod <$> processModImports' modData) $ getModData mod
     where
        -- evalImport wrapper for modData
        processModImports' :: ModData -> St.SysExceptIO ModData
        processModImports' modData = do
            -- update the local mod env with imports
            lEnv' <- DF.foldlM evalImport (modModEnv modData) (modImportCmds modData)
            -- update modData
            return $ modData { modModEnv = lEnv', modImportCmds = [] }

    -- extracts all unit data from module level to global state
    -- TODO - this doesn't need IO
    processModUnits :: Module DesId -> St.SysExceptIO (Module DesId)
    processModUnits mod = do
        case getModData mod of
            Nothing -> return mod
            Just modData -> do
                -- add the data from modData
                -- quantities
                St.modSysState St.vQuantities (\qBimap -> U.addQuantitiesToBimap qBimap (modQuantities modData))
                -- units
                St.modSysStateM St.vUnitDimEnv (\unitDimEnv ->
                    St.liftExSys $ U.addUnitsToEnv unitDimEnv (modUnits modData))
                -- conv defs
                unitDimEnv <- St.getSysState St.vUnitDimEnv
                St.modSysStateM St.vConvEnv (\convEnv ->
                    St.liftExSys $ U.addConvsToGraph convEnv (modConvs modData) unitDimEnv)
                -- clear and return the modData
                return $ putModData mod (modData { modQuantities = [], modUnits = [], modConvs = [] })


-- a basic interpreter over the set of module types, interpres the modules with regards to the moduleenv
evalModDef' :: GlobalModEnv -> FileData -> St.UnitsState -> Module DesId -> MExcept (Module Id)

-- simply looks up the id within both the file and then global env and return the module if found
evalModDef' gModEnv fileData _ mod@(RefMod _ _ _ _) = errorDump [MkSB mod] "Trying to eval a ref module" assert

-- simply looks up the id within both the file and then global env and return the module if found
evalModDef' gModEnv fileData _ mod@(VarMod modName) = do
    (modFullName, mod) <- getModuleFile modName fileData gModEnv -- should this be getRealModuleFile ??
    return $ mkRefMod modFullName mod


-- TODO - how do we merge expected and actual module interfaces??
-- we use an App to convert a Functor Mod eith programmable imports into an Evaled mod
-- where VarMod args are converted to explicit imports, and in-line apps to an internal modEnv (as not used elsewhere)
evalModDef' gModEnv fileData unitsState mod@(AppMod fModId modArgs) = do
    -- lookup all the required modules and dynamically type-check args/sig them
    (modFullName, (FunctorMod fArgs fExprMap fModData)) <- lookupFunctor
    lModEnv <- processFArgs fArgs

    -- now create a dummy lMod from the fMod that would be created from the application of args to the functor
    -- and type and unit check
    let lMod = LitMod fExprMap (updateModData lModEnv fModData)
    (LitMod _ modData') <- typeCheck gModEnv fileData unitsState lMod

    -- finally construct a "Closed" RefMod to holds the results, using both the fMod modData and type-checked lMod sig
    let refMod = RefMod modFullName True (modSigMap modData') lModEnv
    trace' [MkSB (refMod :: Module Id)] "App -> RefMod" $ return refMod
    -- return refMod
  where
    -- lookup/evaluate the functor and params, dynamically type-check
    lookupFunctor :: MExcept (ModFullName, Module Id)
    lookupFunctor = do
        (modFullName, mod) <- getModuleFile fModId fileData gModEnv
        -- can only apply to a Functor, can't apply to an RefMod->FunctorMod
        case mod of
            (FunctorMod _ _ _)              -> return (modFullName, mod)
            (RefMod modFullName1 False _ _) -> derefRefMod mod gModEnv >>= (\mod1 -> return (modFullName1, mod1))
            _                               -> throwError $ printf "(MD01) - Module %s is not a functor" (show fModId)

    -- check the args, they are either inline apps or var lookups
    -- map and sequence thru interpretation of the args, using the same env
    -- create the new moddata for the module, i.e. convert the func args into copied ref-modules within localEnv
    processFArgs :: FunArgs -> MExcept LocalModEnv
    processFArgs funArgs = do
        modArgs <- eModArgs
        DF.foldlM interpretArgs Map.empty modArgs
      where
        -- an assocList of the (modName/argName, mod) for particular functor
        eModArgs = if (OrdMap.size funArgs == length modArgs)
            then return $ zip (OrdMap.keys funArgs) modArgs
            else throwError $ printf "(MD05) - Wrong number of arguments for functor application, expected %d, got %d" (OrdMap.size funArgs) (length modArgs)

        interpretArgs :: LocalModEnv -> (ModName, Module DesId) -> MExcept LocalModEnv
        -- need to eval the module argument and add to the localModEnv
        interpretArgs modEnv (argName, mod) = do
            -- eval the VarMod/AppMod -> RefMod
            mod' <- case mod of
                (VarMod _) -> evalModDef' gModEnv fileData unitsState mod
                (AppMod _ _) -> evalModDef' gModEnv fileData unitsState mod
                -- bomb out, should never happen as args can only ever be Var & App
                _  -> errorDump [MkSB argName, MkSB mod] "Incorrect mod type found as functor arg" assert
            -- now check refMod is closed (thus either a LitMod or a prev. applied FuncMod)
            if isClosedMod mod'
                -- copy the RefMod directly into lModEnv, using the arg name, will have requierd typesig
                then return $ Map.insert argName mod' modEnv
                else throwError $ printf "(MD02) - Module of invalid type used as argument to functor %s" (show fModId)

    updateModData :: LocalModEnv -> ModData -> ModData
    updateModData modModEnv modData = modData { modModEnv = modModEnv }

-- handle both litmods and functor mods
evalModDef' gModEnv fileData unitsState mod = do
    -- reorder - place the expressions from args ahead of thos withing the func module
    -- renaming, use the free vars to deteermine a safe renaimg scheme
    -- typecheck, check the args are valid, then typecheck the signatures, using the same alogirthm as typechecking an app
    mod' <- validate mod >>= rename >>= typeCheck gModEnv fileData unitsState
    return mod'

-- Ref Mod Helper Funcs ------------------------------------------------------------------------------------------------

-- | Take the output from an evaled module and wrap/lift it to an EvaledMod
-- is this partial evaluation??
-- For EvaledMod, LitMod, and FunctorMod, just copy the typesig into an empty modData
-- VarMods not handled, and AppMod handled directy within eval
mkRefMod :: ModFullName -> Module Id -> Module Id
mkRefMod modFullName mod = case mod of
    mod'@(LitMod _ modData)         ->  RefMod modFullName True (modSigMap modData) Map.empty
    mod'@(FunctorMod _ _ modData)   ->  RefMod modFullName False Map.empty Map.empty
    mod'@(RefMod _ _ _ _)           ->  mod' -- just return (a copy) of the same refmod - no need to link the refs
    _ ->  errorDump [MkSB modFullName, MkSB mod] "Can't wrap a RefMod around a module of this type" assert

-- | Dereference a mod
derefRefMod :: Module Id -> GlobalModEnv -> MExcept (Module Id)
derefRefMod mod@(RefMod modFullName _ _ _) gEnv = getModuleGlobal modFullName gEnv
derefRefMod mod _ = return mod

-- | Check is the module is open or closed only applies to the three base Mod Types, App&Var Mods are only ever tmp
isClosedMod :: Module Id -> Bool
isClosedMod (LitMod _ _) = True
isClosedMod (FunctorMod _ _ _) = False
isClosedMod (RefMod _ isClosed _ _) = isClosed


-- Functor Application Helper Funcs ------------------------------------------------------------------------------------
-- TODO - utilise this code later when we unfold all modules into a single block of code

-- actually evaluate the functor applciation, similar to evaluation of function application
applyFunctor :: Module Id -> LocalModEnv -> Module Id
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
        updateVars (Var (ModVar modId id) mRecId)
            | modId == argName = Var (LocalVar (argIdBimap Bimap.! id)) mRecId

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


