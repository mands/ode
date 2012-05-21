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
-- |
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
evalTopElems (st, fd) topMod@(TopMod modRoot modName mod) = do
    modEnv' <- mkExceptIO $ Map.insert <$> pure modName <*> eRes <*> pure modEnv
    return $ (st, fd { fileModEnv = modEnv' })
  where
    modEnv = fileModEnv fd
    eRes :: MExcept (Module E.Id)
    eRes = checkName *> evalModDef modEnv mod
    -- check if module already exists
    checkName = if Map.member modName modEnv then throwError ("(MD06) - Module with name " ++ (show modName) ++ " already defined") else pure ()
    fullModName = mkModFullName (Just modRoot) modName


-- top import, called from REPL or within a file
evalTopElems (st, fd) (ModImport modRoot mMods) =
    if Set.member modRoot parsedFiles -- if we've parsed this file already
        then if Map.member modRoot (stGlobalModEnv st) -- then it should be in global cache
            then do -- so add the imports to the cur filedata
                fd' <- mkExceptIO $ addImportsToFile modRoot mMods st fd
                return (st, fd')
                -- liftIO $ debugM "ode3.modules" ("Importing modules in " ++ show modRoot ++ " from global to local env")
                -- let localEnv' = Map.union (fromJust . Map.lookup modRoot $ stGlobalModEnv st) (stLocalModEnv st)
                -- return $ st { stLocalModEnv = localEnv' }
            else -- ifnot, shit, error, we must be in the process of analysing the file already, is parsed but not global cache
                throwError $ "Modules " ++ show modRoot ++ " have already been parsed, import cycle detected"
        else do -- module not parsed, this should can occur from within REPL or file
            liftIO $ debugM "ode3.modules" ("Searching for modules in " ++ show modRoot)
            -- need to load module
            st' <- processImport st modRoot
            -- now process the import cmd
            fd' <- mkExceptIO $ addImportsToFile modRoot mMods st' fd
            return (st', fd')
  where
    -- parsed Files
    parsedFiles = stParsedFiles st

-- | make aliasName an alias of origName
evalTopElems (st, fd) (ModAlias aliasName origName) =
    -- check aliased module exists, if so then update modEnv
    case Map.member origName modEnv of
        True -> return $ undefined -- (st { stLocalModEnv = mkAlias }, fd)
        False -> throwError $ show origName ++ " not found in current module environment"
  where
    modEnv = undefined -- stLocalModEnv st
    -- insert an alias from Y to X in the modEnv, by creating a new VarMod pointer
    mkAlias = Map.insert aliasName (VarMod (mkModFullName Nothing origName)) modEnv
    -- origName = mkModRoot origElems

-- | Imports the modules from file modRoot to the current fileData
addImportsToFile modRoot mMods st fd = do
    -- get list of modules of imported file
    importedModEnv <- fileModEnv <$> getFileData (stGlobalModEnv st) modRoot
    -- update the cur fd import map with those from the imported modules
    importMap <- DF.foldlM (addImport importedModEnv) (fileImports fd) (modList importedModEnv)
    return $ fd { fileImports = importMap }
  where
    -- create a list of modules to import, either all, or the given selection
    modList importedModEnv = maybe (map (\mod -> (mod, Nothing)) $ Map.keys importedModEnv) id mMods
    -- create an import ref for the given modName, with potential alias
    addImport importedModEnv impMap (modName, mAlias) =
        if Map.member modName importedModEnv
            then return $ Map.insert (maybe modName id mAlias) (mkModFullName (Just modRoot) modName) impMap
            else throwError $ "Imported module " ++ show modName ++ " not found in " ++ show modRoot


-- | Takes a string representing the module URI and returns the path and module name
-- i.e. W.X.Y.Z -> (W/X/Y, Z)
uriToFilePath :: ModRoot -> FilePath
uriToFilePath (ModRoot modRoot) = (FP.makeValid . FP.normalise . FP.joinPath $ modElems) FP.<.> "od3"
  where
    modElems = ListSplit.splitOn "." modRoot

processImport :: ShState -> ModRoot -> MExceptIO ShState
processImport st modRoot = do
    -- load the file
    fileElems <- loadModFile modRoot st
    -- update parsed files cache
    let st' = st { stParsedFiles = Set.insert modRoot (stParsedFiles st) }

    -- pre-emptively load all the referenced imports into the global env
    -- st'' <- loadRefImports modRoot st' pSt

    -- create a new fileData to store the metadata
    let fileData = mkFileData

    -- process the file, having reset the local modEnv
    -- now we're ready to process the elems contained within the file, i.e imports, mod defs, etc. using the local env
    (st'', fileData') <- DF.foldlM evalTopElems (st', fileData) fileElems
    -- have finished the file, so update the global modenv
    liftIO $ debugM "ode3.modules" $ "Finished processing " ++ show modRoot
    return $ st'' { stGlobalModEnv = Map.insert modRoot fileData' (stGlobalModEnv st'') }


-- Loads an individual module file, taking the filename, module root, list of modules to load and current state
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

-- | helper function that takes the list of of improts scanned within a file and processes them, updaing globalModEnv as required
loadRefImports :: ModRoot -> ShState -> CP.PState -> MExceptIO ShState
loadRefImports modRoot st pSt = mapM_ isCycleImport newImports *> DF.foldlM processImport st newImports
  where
    -- new files not yet parsed that we need to import
    newImports = Set.toList $ (CP.stImports pSt) Set.\\ (stParsedFiles st)
    -- cyclar import check, if import file exists in parsedFiles' but not globalEnv then is cycle
    isCycleImport :: ModRoot -> MExceptIO ()
    isCycleImport importRoot = if Set.member importRoot (stParsedFiles st) && Map.notMember importRoot (stGlobalModEnv st)
        then throwError $ "Module " ++ show importRoot ++ ", imported from, " ++ show modRoot ++ "has already been parsed, import cycle detected"
        else return ()
