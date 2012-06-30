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
evalModDef, evalModDef'
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
import Utils.Utils
import qualified Utils.OrdMap as OrdMap
import qualified Utils.OrdSet as OrdSet
import qualified UI.SysState as St

import Lang.Common.AST
import Lang.Module.AST
import Lang.Core.AST

--import Core.Reorderer (reorder)
import Lang.Core.Renamer (rename)
import Lang.Core.Validator (validate)
import Lang.Core.TypeChecker (typeCheck)
import {-# SOURCE #-} Lang.Module.ModCmdDriver (evalImport) -- special import to break import cycle

-- Evaluate Module Defintions ------------------------------------------------------------------------------------------

evalModDef' :: FileData -> Module DesId -> St.SysExceptIO (Module Id)
evalModDef' fd mod = do
    -- process the imports
    mod' <- processModImports mod
    -- extract the units info
    processModUnits mod'
    -- eval the module
    modEnv <- get St.vModEnv <$> S.get
    case evalModDef modEnv fd mod' of
        Left err -> throwError err
        Right mod -> return mod
  where
    -- use [importCmds] to process imports for the module and create an import map, can then validate/typecheck/etc. against it
    processModImports :: Module DesId -> St.SysExceptIO (Module DesId)
    processModImports mod = case mod of
            LitMod exprMap modData -> LitMod exprMap <$> processModImports' modData
            FunctorMod args exprMap modData -> FunctorMod args exprMap <$> processModImports' modData
            otherwise -> return mod
      where
        -- evalImport wrapper for modData
        processModImports' :: ModData -> St.SysExceptIO ModData
        processModImports' modData = do
            st <- S.get
            importMap <- DF.foldlM evalImport Map.empty (modImportCmds modData)
            return $ modData { modImportMap = importMap, modImportCmds = [] }

    -- all units lifting from module level to global state go here too
    processModUnits :: Module DesId -> St.SysExceptIO ()
    processModUnits mod = undefined


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
evalModDef gModEnv fileData mod@(VarMod modName) = snd <$> getModuleFile modName fileData gModEnv

-- TODO - need to update to not actually apply the functor, just link and update the module sigMap
-- we use an App to convert a Functor Mod eith programmable imports into a Lit mod
-- where VarMod args are converted to explicit imports, and in-line apps to an internal modEnv (as not used elsewhere)
evalModDef gModEnv fileData mod@(AppMod fModId modArgs) = do
    -- need to check that the application is valid, if so then create a new module
    -- involves several steps with specialised pipeline operations

    -- reorder - place the expressions from args ahead of thos withing the func module
    -- renaming, use the free vars to deteermine a safe renaimg scheme
    -- typecheck, check the args are valid, then typecheck the signatures, using the same alogirthm as typechecking an app
    -- within an expression, run the same constraint algorithm, then matchup the sigs

    -- order is, args/sig check, typecheck, rename, reorder
    -- should return a new closed module that can be reused later on
    fMod@(FunctorMod fArgs fExprMap fModData) <- eFMod
    (importMap, modEnv) <- processFArgs fArgs

    -- appModEnv <- (getAppModEnv fMod) =<< eModArgs
    -- (fMod', appModEnv') <- typeCheckApp fMod appModEnv
    -- let mod' = applyFunctor fMod' appModEnv'
    -- return $ mod'

    -- now 'eval' the fMod into an lMod using the new modData
    let lMod = LitMod fExprMap (updateModData fModData importMap modEnv)
    -- typcheck the application of args to the functor, get a new sigMap and typeMap
    (gModEnv', lMod') <- typeCheck (gModEnv, lMod)
    return lMod'
  where
    -- lookup/evaluate the functor and params, dynamically type-check
    eFMod :: MExcept (Module Id)
    eFMod = do
        (_, mod) <- getModuleFile fModId fileData gModEnv -- may need getRealModule here?
        case mod of
            (FunctorMod _ _ _) -> return mod
            _ -> throwError $ printf "(MD01) - Module %s is not a functor" (show fModId)

    -- interpret the args, either eval inline apps or lookup
    -- map and sequence thru interpretation of the args, using the same env

--    eModArgs :: MExcept [Module Id]
--    eModArgs = DT.mapM interpretArgs modArgs
--      where
--        interpretArgs argMod = do
--            mod <- evalModDef gModEnv fileData argMod
--            case mod of
--                (LitMod _ _) -> return mod -- should always return a list of litmods
--                _ -> throwError $ printf "(MD03) - Module argument to functor %s is not a literal module" (show fModId)
--
--    -- rename the argMods according to pos/create a new modenv to evalulate the applciation within
--    getAppModEnv :: Module Id -> [Module Id] -> MExcept LocalModEnv
--    getAppModEnv (FunctorMod funArgs _ _) argMods = if (OrdMap.size funArgs == length argMods)
--        then return (Map.fromList $ zip (OrdMap.keys funArgs) argMods)
--        else throwError "(MD05) - Wrong number of arguments for functor application"

    -- create the new moddata for the module, i.e. convert the func args into explict imports/attached-modules
    processFArgs :: FunArgs -> MExcept (ImportMap, LocalModEnv)
    processFArgs funArgs = do
        modArgs <- eModArgs
        DF.foldlM interpretArgs (Map.empty, Map.empty) modArgs
      where
        -- need to convert the var mod into an importMap ref of the correct type
        interpretArgs (importMap, modEnv) (argName, mod@(VarMod modName)) = do
            -- need to look up within file
            (modFullName, _) <- getRealModuleFile modName fileData gModEnv
            -- now add modName->modFullName to importMap
            return (Map.insert modName modFullName importMap, modEnv)
          where
            fModEnv = fileModEnv fileData
            fImportMap = fileImportMap fileData

        -- need eval the Argmod into a LitMod, then add directly into the localModEnv
        interpretArgs (importMap, modEnv) (argName, mod@(AppMod modName modArgs)) = do
            -- eval the Appmod
            mod <- evalModDef gModEnv fileData mod
            -- now insert the litmod into local modEnv using the arg name
            return (importMap, Map.insert argName mod modEnv)

        interpretArgs _ (name, mod) = errorDump [MkSB name, MkSB mod] "Incorrect mod type found as functor arg" -- bomb out, should never happen

        -- an assocList of the (modName/argName, mod) for particular functor
        eModArgs = if (OrdMap.size funArgs == length modArgs)
            then return $ zip (OrdMap.keys funArgs) modArgs
            else throwError $ printf "(MD05) - Wrong number of arguments for functor application, expected %d, got %d" (OrdMap.size funArgs) (length modArgs)

    updateModData :: ModData -> ImportMap -> LocalModEnv -> ModData
    updateModData fModData importMap modModEnv =
        fModData    { modImportMap = Map.union importMap (modImportMap fModData) -- functor imports take precedence
                    , modLocalModEnv = modModEnv
                    }


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


