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

module Lang.Module.ModuleDriver (
evalTopElems
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

import qualified System.FilePath as FP
import qualified System.Directory as Dir


import Lang.Module.Parser


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

-- | Takes a string representing the module URI and returns the path and module name
-- i.e. W.X.Y.Z -> (W/X/Y, Z)
uriToPath :: ModURIElems -> FilePath
uriToPath modElems = (FP.makeValid . FP.normalise . FP.joinPath $ modElems) FP.<.> "od3"

-- | Flattens a list of URI elems into dot-notation
flattenURI :: ModURIElems -> ModURI
flattenURI = List.intercalate "."

-- | takes a list of URI elems and modulename into a dot-notation
mkModName :: ModURIElems -> ModURI -> ModURI
mkModName modRootURI indivName = (flattenURI modRootURI) ++ "." ++ indivName


-- | Main console and file evaluater
-- takes the current state and processes the given top command/def against it
evalTopElems :: ShState -> OdeTopElem E.DesId -> MExceptIO ShState
evalTopElems st topMod@(TopMod name mod) = do
    modEnv' <- mkExceptIO $ Map.insert <$> pure canonName <*> eRes <*> pure modEnv
    return $ st { stModuleEnv = modEnv' }
  where
    eRes :: MExcept (Module E.Id)
    eRes = checkName *> evalModDef modEnv mod
    -- check if module already exists
    checkName = if Map.member name modEnv then throwError ("(MD06) - Module with name " ++ (show name) ++ " already defined") else pure ()
    canonName = List.intercalate "." [canonRoot, name]
    modEnv = stModuleEnv st

    -- HACK
    canonRoot = "unknown"


evalTopElems st (ModImport modRootElems Nothing) =
    -- import all modules from file
    -- check if file already loaded via FP and canon name in modEnv
    if Set.member filePath parsedFiles
        then liftIO $ debugM "ode3.modules" ("Modules in " ++ filePath ++ " already loaded, ignoring") >> return st
        else do
            liftIO $ debugM "ode3.modules" ("Modules in " ++ filePath ++ " not found, loading")
            fileElems <- loadModFile filePath canonRoot Nothing st

            -- TODO - fix the order
            -- update cache
            let st' = st { stParsedFiles = Set.insert filePath parsedFiles }

            -- do we recurse here??
            st'' <- DF.foldlM evalTopElems st' fileElems
            return $ st''
  where
    -- get canon name for root
    canonRoot = flattenURI modRootElems
    -- get FP
    filePath = uriToPath modRootElems
    -- parsed Files
    parsedFiles = stParsedFiles st


evalTopElems st (ModImport modRootElems (Just mods)) = do
    -- see if canon name is already in modEnv
    -- just load individual module (actually for now we load everything anyway)
    st' <- if null modsLoad
        then liftIO $ debugM "ode3.modules" ("Modules " ++ show (map fst mods) ++ " already loaded into modEnv, ignoring") >> return st
        else do
            liftIO $ debugM "ode3.modules" ("Modules " ++ show modsLoad ++ " not found in modEnv, loading")
            fileElems <- loadModFile filePath canonRoot (Just modsLoad) st
            -- update cache
            -- HACK - we assume that we loaded all modules for a file for now and update cache accordingly
            let st' = st { stParsedFiles = Set.insert filePath (stParsedFiles st') }

            -- do we recurse here??
            st'' <- DF.foldlM evalTopElems st' fileElems
            return $ st''

    -- setup the alias if needed
    st'' <- DF.foldlM evalTopElems st' aliasCmds

    return st''
  where
    -- get canon name
    canonRoot = flattenURI modRootElems
    -- get FP
    filePath = uriToPath modRootElems
    -- set of modules to load from file
    modsLoad :: [ModURI]
    modsLoad = do
        let modEnv = (stModuleEnv st)
        (m, _) <- mods
        let modName = mkModName modRootElems m
        guard $ Map.notMember modName modEnv
        return m
    -- list of modules to alias
    aliasCmds = [ ModAlias (fromJust a) (modRootElems ++ [m]) | (m, a) <- mods, isJust a == True ]
    -- parsed Files
    parsedFiles = stParsedFiles st


-- | make aliasName an alias of origName
evalTopElems st (ModAlias aliasName origElems) =
    -- check aliased module exists, if so then update modEnv
    case Map.member origName modEnv of
        True -> return $ st { stModuleEnv = mkAlias }
        False -> throwError $ show origName ++ " not found in current module environment"
  where
    modEnv = stModuleEnv st
    -- insert an alias from Y to X in the modEnv, by creating a new VarMod pointer
    mkAlias = Map.insert aliasName (VarMod origName) modEnv
    origName = flattenURI origElems



-- Loads an individual module file, taking the filename, module root, list of modules to load and current state
loadModFile :: FilePath -> ModURI -> Maybe [ModURI] -> ShState -> MExceptIO ([OdeTopElem E.DesId])
loadModFile filePath canonRoot _ st = do
    mFileExist <- liftIO repoFileSearch
    case mFileExist of
        Nothing -> throwError $ "File " ++ show filePath ++ " not found in any module repositories"
                --debugM "ode3.modules" $ "File " ++ show filePath ++ " not found in any module repositories"
        Just modFilePath -> do
            -- need to load module - pass the data to orig mod parser
            liftIO $ debugM "ode3.modules" $ "Module found in " ++ modFilePath
            -- load the contents of the file
            modFileData <- liftIO $ readFile modFilePath
            -- st' :: MExcept ShState
            -- have to explicty case from Error to ErrorT monad
            mkExceptIO $ fileParse modFilePath modFileData canonRoot
  where
    -- search for filePath exists roots
    repoFileSearch :: IO (Maybe FilePath)
    repoFileSearch = liftM DF.msum $ mapM checkFile (OrdSet.toList $ stRepos st)

    checkFile :: FilePath -> IO (Maybe FilePath)
    checkFile repo = Cond.ifM (Dir.doesFileExist repoFilePath) (return $ Just repoFilePath) (return $ Nothing)
      where
        repoFilePath = repo FP.</> filePath



-- a basic interpreter over the set of module types, interpres the modules with regards tro the moduleenv
evalModDef :: ModuleEnv -> Module E.DesId -> MExcept (Module E.Id)
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

-- simply lookup the id within the env and return the module
evalModDef modEnv mod@(VarMod modId) = case (Map.lookup modId modEnv) of
    Just mod -> return mod
    Nothing -> throwError $ "(MD07) - Referenced module " ++ (show modId) ++ " not found in envirnoment"

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
            _ -> throwError ("(MD01) - Module " ++ fModId ++ " is not a functor")
        Nothing -> throwError ("(MD02) - Functor module " ++ fModId ++ " not found")

    -- interpret the args, either eval inline apps or lookup
    -- map and sequence thru interpretation of the args, using the same env
    eArgMods :: MExcept [Module E.Id]
    eArgMods = DT.mapM interpretArgs argMods
      where
        interpretArgs argMod = do
            mod <- evalModDef modEnv argMod
            case mod of
                (LitMod _ _) -> return mod
                _ -> throwError ("(MD03) - Module argument to functor " ++ fModId ++ " is not a literal module")

    -- rename the argMods according to pos/ create a new modenv to evalulate the applciation within
    getAppModEnv :: Module E.Id -> [Module E.Id] -> MExcept ModuleEnv
    getAppModEnv (FunctorMod funArgs _ _) argMods = if (OrdMap.size funArgs == length argMods)
        then return (Map.fromList $ zip (OrdMap.keys funArgs) argMods)
        else throwError "(MD05) - Wrong number of arguments for functor application"

-- actually evaluate the functor applciation, similar to evaluation of function application
applyFunctor :: Module E.Id -> ModuleEnv -> Module E.Id
applyFunctor fMod@(FunctorMod fArgs fExprMap fModData) modEnv = resMod
  where
    -- best way to do this, create an empty lit mod, and add to it all the data by folding over the modEnv
    baseMod = LitMod fExprMap fModData
    resMod = Map.foldrWithKey appendModule baseMod modEnv

-- adds one module into another, basically mappend
appendModule :: E.SrcId -> Module E.Id -> Module E.Id -> Module E.Id
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
appendModuleData :: ModuleData -> ModuleData -> ModuleData
appendModuleData argModData baseModData = baseModData { modTMap = typeMap', modIdBimap = idBimap', modFreeId = freeId' }
  where
    deltaId = fromJust $ (modFreeId argModData)
    typeMap' = Map.union (modTMap argModData) (Map.mapKeys (+ deltaId) (modTMap baseModData)) -- update the base ids and merge
    idBimap' = Bimap.fromList . map (\(srcId, id) -> (srcId, id + deltaId)) . Bimap.toList . modIdBimap $ baseModData   -- update the base ids only
    freeId' = liftM2 (+) (modFreeId argModData) (modFreeId baseModData) -- sum of both numbers


