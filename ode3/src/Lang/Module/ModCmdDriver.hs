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
import qualified System.Directory as Dir
import qualified System.FilePath as FP

import Data.Maybe (isJust, fromJust)
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
import Utils.Utils
import UI.ShellState

import Lang.Module.Parser
import qualified Lang.Common.Parser as CP
import Lang.Common.AST
import Lang.Module.AST
import qualified Lang.Core.AST as E
import Lang.Module.ModDefDriver

-- | Main console and file evaluater
-- takes the current state and processes the given top command/def against it
evalTopElems :: (ShState, FileData) -> OdeTopElem E.DesId -> MExceptIO (ShState, FileData)
evalTopElems (st, fd) topMod@(TopModDef modRoot modName mod) = do
    checkName
    (st', mod') <- processModImports -- import any ref'd modules here first
    mod'' <- mkExceptIO $ evalModDef modEnv mod' -- eval the actual mod def
    return $ (st', fd { fileModEnv = Map.insert modName mod'' modEnv }) -- insert the module into the file mod env
  where
    modEnv = fileModEnv fd
    -- check if module already exists
    checkName = if Map.member modName modEnv then throwError ("(MD06) - Module with name " ++ (show modName) ++ " already defined") else pure ()

    -- TODO - should this be here or in evalModDef?
    -- use importList to then process imports and create an import map for the module, can then validate/typecheck/etc. against it
    processModImports :: MExceptIO (ShState, Module E.DesId)
    processModImports = case mod of
            LitMod exprMap modData -> mapSnd <$> pure (LitMod exprMap) <*> processImports' modData
            FunctorMod args exprMap modData -> mapSnd <$> pure (FunctorMod args exprMap) <*> processImports' modData
            otherwise -> return (st, mod)
      where
        -- evalImport wrapper for modData
        processImports' :: ModData -> MExceptIO (ShState, ModData)
        processImports' modData =
            mapSnd <$> pure (\importMap -> modData { modImportMap = importMap }) <*> DF.foldlM evalImport (st, Map.empty) (modImportCmds modData)


-- top import, called from REPL or within a file
evalTopElems (st, fd) (TopModImport importCmd@(ModImport modRoot mMods)) =
    mapSnd <$> pure (\importMap -> fd {fileImportMap = importMap }) <*> evalImport (st, (fileImportMap fd)) importCmd

-- Main function to eval an import command with respect to the system state
evalImport :: (ShState, ImportMap) -> ModImport -> MExceptIO (ShState, ImportMap)
evalImport (st, importMap) importCmd@(ModImport modRoot _) = do
    if Set.member modRoot parsedFiles -- if we've parsed this file already
        then if Map.member modRoot (stGlobalModEnv st) -- then it should be in global cache
            then do -- so add the imports to the cur filedata
                importMap' <- addImport st importMap
                return (st, importMap')
            else -- ifnot, shit, error, we must be in the process of analysing the file already, is parsed but not global cache
                throwError $ "Modules " ++ show modRoot ++ " have already been parsed, import cycle detected"
        else do -- module not parsed, this should can occur from within REPL, top file, or module
            liftIO $ debugM "ode3.modules" ("Searching for modules in " ++ show modRoot)
            -- need to load module
            st' <- loadImport st modRoot
            -- now process the import cmd
            importMap' <- addImport st' importMap
            return (st', importMap')
  where
    -- parsed Files
    parsedFiles = stParsedFiles st
    -- add import wrapper
    addImport st importMap = mkExceptIO $ addImportsToMap importCmd st importMap

-- | Imports the modules cmds into the given importmap
addImportsToMap :: ModImport -> ShState -> ImportMap -> MExcept ImportMap
addImportsToMap (ModImport modRoot mMods) st importMap = do
    -- get list of modules of imported file
    importedModEnv <- fileModEnv <$> getFileData (stGlobalModEnv st) modRoot
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
loadImport :: ShState -> ModRoot -> MExceptIO ShState
loadImport st modRoot = do
    -- load the file
    fileElems <- loadModFile modRoot st
    -- update parsed files cache
    let st' = st { stParsedFiles = Set.insert modRoot (stParsedFiles st) }
    -- create a new fileData to store the metadata
    let fileData = mkFileData
    -- process the file, having reset the local modEnv
    -- now we're ready to process the elems contained within the file, i.e imports, mod defs, etc. using the local env
    (st'', fileData') <- DF.foldlM evalTopElems (st', fileData) fileElems
    -- have finished the file, so update the global modenv using the modified fileData'
    liftIO $ debugM "ode3.modules" $ "Finished processing " ++ show modRoot
    return $ st'' { stGlobalModEnv = Map.insert modRoot fileData' (stGlobalModEnv st'') }


-- Actually loads an individual file of modules from a module root
loadModFile :: ModRoot -> ShState -> MExceptIO [OdeTopElem E.DesId]
loadModFile modRoot st = do
    mFileExist <- liftIO repoFileSearch
    case mFileExist of
        Nothing -> throwError $ "File " ++ show filePath ++ " not found in any module repositories"
                --debugM "ode3.modules" $ "File " ++ show filePath ++ " not found in any module repositories"
        Just modFilePath -> do
            -- need to load module - pass the data to orig mod parser
            liftIO $ debugM "ode3.modules" $ "Modules may be found in file " ++ modFilePath
            -- load the contents of the file
            modFileData <- liftIO $ readFile modFilePath
            -- st' :: MExcept ShState
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
