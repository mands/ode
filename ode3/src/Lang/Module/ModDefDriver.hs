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

{-# LANGUAGE FlexibleContexts #-}

module Lang.Module.ModDefDriver (
evalModDef
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
import qualified SysState as St

import Lang.Common.AST
import Lang.Module.AST
import Lang.Core.AST
import qualified Lang.Core.Units as U
import {-# SOURCE #-} Lang.Module.ModCmdDriver (evalImport) -- special import to break import cycle

--import Core.Reorderer (reorder)
import Lang.Core.Renamer (rename)
import Lang.Core.Validator (validate)
import Lang.Core.TypeChecker (typeCheck)

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
            -- calc new importMap
            importMap <- DF.foldlM evalImport Map.empty (modImportCmds modData)
            -- update modData
            return $ modData { modImportMap = importMap, modImportCmds = [] }

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
evalModDef' gModEnv fileData _ mod@(RefMod _ modFullName _) = errorDump [MkSB mod] "Trying to eval a previously evaled module" assert

-- simply looks up the id within both the file and then global env and return the module if found
evalModDef' gModEnv fileData _ mod@(VarMod modName) = do
    (modFullName, mod) <- getModuleFile modName fileData gModEnv -- should this be getRealModuleFile ??
    return $ mkRefMod modFullName mod


-- TODO - how do we merge expected and actual module interfaces??
-- we use an App to convert a Functor Mod eith programmable imports into an Evaled mod
-- where VarMod args are converted to explicit imports, and in-line apps to an internal modEnv (as not used elsewhere)
evalModDef' gModEnv fileData unitsState mod@(AppMod fModId modArgs) = do

    -- reorder - place the expressions from args ahead of thos withing the func module
    -- renaming, use the free vars to deteermine a safe renaimg scheme
    -- typecheck, check the args are valid, then typecheck the signatures, using the same alogirthm as typechecking an app
    -- within an expression, run the same constraint algorithm, then matchup the sigs

    -- order is, args/sig check, typecheck, rename, reorder
    -- should return a new evaled module that can be reused later on
    (modFullName, (FunctorMod fArgs fExprMap fModData)) <- eFMod
    (importMap, modEnv) <- processFArgs fArgs

    -- now create a dummy lMod from the fMod that would be created from the application of args to the functor
    -- and type and unit check
    let lMod = LitMod fExprMap (updateModData importMap modEnv fModData)
    lMod' <- typeCheck gModEnv fileData unitsState lMod

    -- finally construct a "Closed" RefMod to holds the results, using both the fMod modData and type-checked lMod sig
    let eMod = modifyModData (mkRefMod modFullName lMod') (updateModData importMap modEnv)
    -- let eMod = (EvaledMod modFullName fModData :: Module Id)
    trace' [MkSB eMod] "eMod" $ return eMod
  where
    -- lookup/evaluate the functor and params, dynamically type-check
    eFMod :: MExcept (ModFullName, Module Id)
    eFMod = do
        -- getModuleFile rather than getRealModuleFile should be fine here as we never allow chaining of functors
        (modFullName, mod) <- getModuleFile fModId fileData gModEnv
        -- can only apply to a Functor, can't apply to an EvaledMod->FunctorMod
        case trace' [MkSB mod] "eFMod" mod of
            (FunctorMod _ _ _) -> return (modFullName, mod)
            -- NOTE - we don't update the modFullName to point to the orig functor in case of a RefMod
            rMod@(RefMod False _ _) ->  derefRefMod mod gModEnv >>= (\mod1 -> return (modFullName, mod1))
            _ -> throwError $ printf "(MD01) - Module %s is not a functor" (show fModId)

    -- check the args, they are either inline apps or var lookups
    -- map and sequence thru interpretation of the args, using the same env
    -- create the new moddata for the module, i.e. convert the func args into explict imports/attached-modules
    processFArgs :: FunArgs -> MExcept (ImportMap, LocalModEnv)
    processFArgs funArgs = do
        modArgs <- eModArgs
        DF.foldlM interpretArgs (Map.empty, Map.empty) modArgs
      where

--        interpretArgs (importMap, modEnv) (argName, mod@(RefMod _ _ _)) = do
--            undefined

        -- need to convert the var mod into an importMap ref of the correct type
        interpretArgs (importMap, modEnv) (argName, mod@(VarMod modName)) = do
            -- need to look up within file
            (modFullName, mod) <- getRealModuleFile modName fileData gModEnv
            return (Map.insert argName modFullName importMap, modEnv)
          where
            fModEnv = fileModEnv fileData
            fImportMap = fileImportMap fileData

        -- need eval the Argmod into a LitMod, then add directly into the localModEnv
        interpretArgs (importMap, modEnv) (argName, mod@(AppMod modName modArgs)) = do
            -- eval the Appmod
            mod <- evalModDef' gModEnv fileData unitsState mod
            -- now insert the litmod into local modEnv using the arg name
            return (importMap, Map.insert argName mod modEnv)

        -- bomb out, should never happen
        interpretArgs _ (name, mod) = errorDump [MkSB name, MkSB mod] "Incorrect mod type found as functor arg" assert

        -- an assocList of the (modName/argName, mod) for particular functor
        eModArgs = if (OrdMap.size funArgs == length modArgs)
            then return $ zip (OrdMap.keys funArgs) modArgs
            else throwError $ printf "(MD05) - Wrong number of arguments for functor application, expected %d, got %d" (OrdMap.size funArgs) (length modArgs)

    updateModData :: ImportMap -> LocalModEnv -> ModData -> ModData
    updateModData importMap modModEnv modData =
        modData    { modImportMap = Map.union importMap (modImportMap modData) -- initital imports take precedence
                    , modLocalModEnv = modModEnv
                    }

-- handle both litmods and functor mods
evalModDef' gModEnv fileData unitsState mod = do
    -- reorder, rename and typecheck the expressinons within module, adding to the module metadata
    mod' <- validate mod >>= rename >>= typeCheck gModEnv fileData unitsState
    return mod'

-- Ref Mod Helper Funcs ------------------------------------------------------------------------------------------------

-- | Take the output from an evaled module and wrap/lift it to an EvaledMod
-- is this partial evaluation??
-- For EvaledMod, LitMod, and FunctorMod, just copy the typesig into an empty modData
-- VarMods not handled, and AppMod handled directy within eval
mkRefMod :: ModFullName -> Module Id -> Module Id
mkRefMod modFullName mod = case mod of
    mod'@(LitMod _ modData) ->  RefMod True modFullName $ wrapModData modData
    mod'@(FunctorMod _ _ modData) ->  RefMod False modFullName $ wrapModData modData
    mod'@(RefMod isClosed' _ modData) ->  RefMod isClosed' modFullName $ wrapModData modData
    _ ->  errorDump [MkSB modFullName, MkSB mod] "Can't wrap into EvaledMod a module of this type" assert
  where
    -- copy across the modSig
    wrapModData :: ModData -> ModData
    wrapModData modData = mkModData { modSig = (modSig modData) }

-- | Repeatdly lookup an evaled module, chaining together the associated modData as required
-- unionData bool indicates wheater we both to union the modData or just drop it till the last refMod
-- (we can union them as shoudl only ever be a single indriection that changes the modData, rest will be blank)
collapseRefMods :: Bool -> Module Id -> GlobalModEnv -> MExcept (Module Id)
collapseRefMods unionData mod1@(RefMod isClosed1 modFullName1 modData1) gEnv = case getModuleGlobal modFullName1 gEnv of
        Right mod2@(RefMod isClosed2 modFullName2 modData2) | isClosed1 == isClosed2 -> if unionData
            then collapseRefMods unionData (RefMod isClosed2 modFullName2 $ unionEModData modData1 modData2) gEnv
            else collapseRefMods unionData mod2 gEnv
        Right mod2@(RefMod _ _ _) -> errorDump [MkSB mod1, MkSB mod2] "(MD) Found chain of referenced modules with differing closed states" assert
        Right mod1 -> return mod1
        Left err -> throwError err
  where
    unionEModData modData1 modData2 = modData2  { modImportMap = Map.union (modImportMap modData1) (modImportMap modData2)
                                                , modLocalModEnv = Map.union (modLocalModEnv modData1) (modLocalModEnv modData2)
                                                }

-- | Dereference a mod contiously till its final mod
derefRefMod :: Module Id -> GlobalModEnv -> MExcept (Module Id)
derefRefMod mod@(RefMod _ modFullName _) gEnv = getModuleGlobal modFullName gEnv >>= (\mod -> derefRefMod mod gEnv)
derefRefMod mod _ = return mod


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


