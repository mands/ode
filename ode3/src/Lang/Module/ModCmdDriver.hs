-----------------------------------------------------------------------------
--
-- Module      :  Lang.Module.ModCmdDriver
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Main module deriver for the top-level commands in a module, i.e. imports and module defitions
--
-- TODO
-- * Only handles a single error, i.e. a not-found qualified import will not save all modules within the modRoot - need to catch earlier
--
-----------------------------------------------------------------------------

module Lang.Module.ModCmdDriver (
evalTopElems
) where


import System.Log.Logger

import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Error
import qualified Control.Conditional as Cond

import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Utils.OrdSet as OrdSet
import qualified Data.Set as Set
import qualified Data.List.Split as ListSplit
import Data.Maybe (isJust, fromJust)

import qualified System.Directory as Dir
import qualified System.FilePath as FP

import Utils.Utils
import UI.SysState

import Lang.Module.Parser
import Lang.Common.AST
import Lang.Module.AST
import Lang.Core.AST
import Lang.Module.ModDefDriver


-- Top Command Evaluation ----------------------------------------------------------------------------------------------

-- | Main console and file evaluater
-- takes the current state and processes the given top command/def against it
evalTopElems :: (SysState, FileData) -> OdeTopElem DesId -> MExceptIO (SysState, FileData)
evalTopElems (st, fd) topMod@(TopModDef modRoot modName mod) = do
    checkName
    (st', mod') <- processModImports -- import any ref'd modules here first
    mod'' <- mkExceptIO $ evalModDef (stGlobalModEnv st) fd mod' -- eval the actual mod def, need to pass global and file modEnvs
    return $ updateState st' fd mod'' -- insert the module into the file mod env
  where
    modEnv = fileModEnv fd -- we use the filemodEnv
    -- check if module already exists in fileModEnv
    checkName = if Map.member modName (fileModEnv fd) then throwError ("(MD06) - Module with name " ++ (show modName) ++ " already defined") else pure ()

    -- TODO - should this be here or in evalModDef?
    -- use importList to then process imports for the module and create an import map, can then validate/typecheck/etc. against it
    processModImports :: MExceptIO (SysState, Module DesId)
    processModImports = case mod of
            LitMod exprMap modData -> mapSnd <$> pure (LitMod exprMap) <*> processModImports' modData
            FunctorMod args exprMap modData -> mapSnd <$> pure (FunctorMod args exprMap) <*> processModImports' modData
            otherwise -> return (st, mod)
      where
        -- evalImport wrapper for modData
        processModImports' :: ModData -> MExceptIO (SysState, ModData)
        processModImports' modData =
            mapSnd <$> pure (\importMap -> modData { modImportMap = importMap, modImportCmds = [] }) <*> DF.foldlM evalImport (st, Map.empty) (modImportCmds modData)

    -- add to both fileData and globalmodenv, so subsequent modules within file can access this module
    updateState :: SysState -> FileData -> Module Id -> (SysState, FileData)
    updateState st fd mod = (st', fd')
      where
        fd' = fd { fileModEnv = Map.insert modName mod (fileModEnv fd) }
        st' = st { stGlobalModEnv = Map.insert modRoot fd (stGlobalModEnv st) }

-- top import, called from REPL or within a file
evalTopElems (st, fd) (TopModImport importCmd@(ModImport modRoot mMods)) =
    mapSnd <$> pure (\importMap -> fd {fileImportMap = importMap }) <*> evalImport (st, (fileImportMap fd)) importCmd


-- Module Importing Evaluation -----------------------------------------------------------------------------------------

-- | Main function to eval an import command with respect to the system state
evalImport :: (SysState, ImportMap) -> ModImport -> MExceptIO (SysState, ImportMap)
evalImport (st, importMap) importCmd@(ModImport modRoot _) = do
    if Set.member modRoot parsedFiles -- if we've parsed this file already
        then if Map.member modRoot (stGlobalModEnv st) -- then it should be in global cache
            then do -- so add the imports to the cur filedata
                importMap' <- mkExceptIO $ addImportsToMap st importMap importCmd
                return (st, importMap')
            else -- ifnot, shit, error, we must be in the process of analysing the file already, is parsed but not global cache
                throwError $ "Modules " ++ show modRoot ++ " have already been parsed, import cycle detected"
        else do -- module not parsed, this should can occur from within REPL, top file, or module
            liftIO $ debugM "ode3.modules" ("Searching for modules in " ++ show modRoot)
            -- need to load module
            st' <- loadImport st modRoot
            -- now process the import cmd
            importMap' <- mkExceptIO $ addImportsToMap st importMap importCmd
            return (st', importMap')
  where
    -- parsed Files
    parsedFiles = stParsedFiles st

-- | Imports the modules cmds into the given importmap
addImportsToMap :: SysState -> ImportMap -> ModImport -> MExcept ImportMap
addImportsToMap st importMap (ModImport modRoot mMods) = do
    -- get list of modules of imported file
    importedModEnv <- fileModEnv <$> getFileData modRoot (stGlobalModEnv st)
    -- update the cur fd import map with those from the imported modules
    DF.foldlM (addImport importedModEnv) importMap (modList importedModEnv)
    -- return $ fd { fileImportMap = importMap }
  where
    -- create a list of modules to import, either all, or the given selection
    modList importedModEnv = maybe (map (\mod -> (mod, Nothing)) $ Map.keys importedModEnv) id mMods
    -- create an import ref for the given modName, with potential alias
    addImport importedModEnv impMap (modName, mAlias) =
        if Map.member modName importedModEnv
            then return $ Map.insert (maybe modName id mAlias) (mkModFullName (Just modRoot) modName) impMap
            else throwError $ "Imported module " ++ show modName ++ " not found in " ++ show modRoot

-- | High level fuction to load a module specified by ModRoot and process it according to the Glboal state
loadImport :: SysState -> ModRoot -> MExceptIO SysState
loadImport st modRoot = do
    -- load the file
    fileElems <- loadModFile st modRoot
    -- update parsed files cache
    let st' = st { stParsedFiles = Set.insert modRoot (stParsedFiles st) }
    -- create a new fileData to store the metadata
    let fileData = mkFileData modRoot
    -- process the file, having reset the local modEnv
    -- now we're ready to process the elems contained within the file, i.e imports, mod defs, etc. using the local env
    (st'', fileData') <- DF.foldlM evalTopElems (st', fileData) fileElems
    -- have finished the file, so update the global modenv using the modified fileData'
    liftIO $ debugM "ode3.modules" $ "Finished processing " ++ show modRoot
    return $ st'' { stGlobalModEnv = Map.insert modRoot fileData' (stGlobalModEnv st'') }


-- Actually loads an individual file of modules from a module root
loadModFile :: SysState -> ModRoot -> MExceptIO [OdeTopElem DesId]
loadModFile st modRoot = do
    mFileExist <- liftIO repoFileSearch
    case mFileExist of
        Nothing -> throwError $ "File " ++ show filePath ++ " not found in any module repositories"
                --debugM "ode3.modules" $ "File " ++ show filePath ++ " not found in any module repositories"
        Just modFilePath -> do
            -- need to load module - pass the data to orig mod parser
            liftIO $ debugM "ode3.modules" $ "Modules may be found in file " ++ modFilePath
            -- load the contents of the file
            modFileData <- liftIO $ readFile modFilePath
            -- st' :: MExcept SysState
            -- have to explicty case from Error to ErrorT monad
            mkExceptIO $ fileParse modFilePath modFileData modRoot
  where
    -- search for filePath exists roots
    repoFileSearch :: IO (Maybe FilePath)
    repoFileSearch = liftM DF.msum $ mapM checkFile (OrdSet.toList $ stRepos st)

    checkFile :: FilePath -> IO (Maybe FilePath)
    checkFile repo = Cond.ifM (Dir.doesFileExist repoFilePath) (return $ Just repoFilePath) (return $ Nothing)
      where
        repoFilePath = repo FP.</> filePath
    -- get FP
    filePath = uriToFilePath modRoot


-- | Takes a string representing the module URI and returns the path and module name
-- i.e. W.X.Y.Z -> (W/X/Y, Z)
uriToFilePath :: ModRoot -> FilePath
uriToFilePath (ModRoot modRoot) = (FP.makeValid . FP.normalise . FP.joinPath $ modElems) FP.<.> "od3"
  where
    modElems = ListSplit.splitOn "." modRoot
