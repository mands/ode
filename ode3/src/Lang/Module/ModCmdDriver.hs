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
evalTopElems, evalImport
) where


-- higher-level control
import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Error
import qualified Control.Monad.State as S
import qualified Control.Conditional as Cond

-- fclabels stuff
import Control.Category
import Data.Label
import Prelude hiding ((.), id)

-- containers
import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Utils.OrdSet as OrdSet
import qualified Data.Set as Set
import qualified Data.List.Split as ListSplit
import Data.Maybe (isJust, fromJust)

-- other
import Text.Printf (printf)
import System.Log.Logger
import qualified System.Directory as Dir
import qualified System.FilePath as FP

-- Ode
import Utils.Utils
import qualified SysState as St

import Lang.Module.Parser
import Lang.Common.AST
import Lang.Module.AST
import Lang.Core.AST
import Lang.Module.ModDefDriver

-- Top Command Evaluation ----------------------------------------------------------------------------------------------

-- | Main console and file evaluater
-- takes the current state and processes the given top command/def against it
-- this is either a module definition, or a file-based module import
evalTopElems :: FileData -> OdeTopElem DesId -> St.SysExceptIO FileData
evalTopElems fd topMod@(TopModDef modRoot modName mod) = do
    checkName
    mod' <- evalModDef fd mod -- eval the actual mod def, need to pass global state and file modEnvs
    updateState fd mod' -- insert the updated module into the file mod env
  where
    -- check if module already exists in fileModEnv
    checkName = if Map.member modName (fileModEnv fd)
        then throwError (printf "(MD06) - Module with name %s already defined" $ show modName) else pure ()

    -- add to both fileData and globalmodenv, so subsequent modules within file can access this module
    updateState :: FileData -> Module Id -> St.SysExceptIO FileData
    updateState fd mod = do
        trace' [MkSB fd, MkSB mod] "updateState" $ return ()
        let fd' = fd { fileModEnv = Map.insert modName mod (fileModEnv fd) }
        -- only update global modEnv if not the console fileMod
        if (fileModRoot fd) == replModRoot
            then return ()
            else St.modSysState St.vModEnv (\modEnv -> Map.insert modRoot fd' modEnv)

        return fd'

-- top import, called from REPL or within a file
evalTopElems fd (TopModImport importCmd@(ModImport modRoot mMods)) = do
    importMap <- evalImport (fileImportMap fd) importCmd
    return $ fd {fileImportMap = importMap }

-- Module Importing Evaluation -----------------------------------------------------------------------------------------

-- | Main function to eval an import command with respect to the system state
evalImport :: ImportMap -> ModImport -> St.SysExceptIO ImportMap
evalImport importMap importCmd@(ModImport modRoot _) = do
    st <- S.get
    if Set.member modRoot (get St.vParsedFiles st) -- if we've parsed this file already
        then if Map.member modRoot (get St.vModEnv st) -- then it should be in global cache
            then -- so add the imports to the cur filedata
                addImportsToMap importMap importCmd
            else -- ifnot, shit, error, we must be in the process of analysing the file already, is parsed but not global cache
                throwError $ printf "Modules %s have already been parsed, import cycle detected" (show modRoot)
        else do -- module not parsed, this should can occur from within REPL, top file, or module
            -- liftIO $ debugM "ode3.modules" ("Searching for modules in " ++ show modRoot)
            -- need to load module
            loadImport modRoot
            -- now process the import cmd
            addImportsToMap importMap importCmd

-- | Imports the modules cmds into the given importmap
-- TODO - this doesn't need IO
addImportsToMap :: ImportMap -> ModImport -> St.SysExceptIO ImportMap
addImportsToMap importMap (ModImport modRoot mMods) = do
    -- get list of modules of imported file
    modEnv <- (St.getSysState St.vModEnv)
    importedModEnv <- fileModEnv <$> (St.liftExSys $ getFileData modRoot modEnv)
    -- update the cur fd import map with those from the imported modules
    DF.foldlM (addImport importedModEnv) importMap (modList importedModEnv)
    -- return $ fd { fileImportMap = importMap }
  where
    -- create a list of modules to import, either all, or the given selection
    modList importedModEnv = maybe (map (\mod -> (mod, Nothing)) $ Map.keys importedModEnv) id mMods
    -- create an import ref for the given modName, with potential alias
    addImport importedModEnv impMap (modName, mAlias) =
        if Map.member modName importedModEnv
            then return $ Map.insert (maybe modName id mAlias) (ModFullName modRoot modName) impMap
            else throwError $ printf "Imported module %s not found in %s" (show modName) (show modRoot)

-- | High level fuction to load a module specified by ModRoot and process it according to the Glboal state
loadImport :: ModRoot -> St.SysExceptIO ()
loadImport modRoot = do
    -- load the file
    fileElems <- loadModFile modRoot
    -- update parsed files cache
    St.modSysState St.vParsedFiles (\files -> Set.insert modRoot files)
    -- create a new fileData to store the metadata
    let fileData = mkFileData modRoot
    -- process the file, having reset the local modEnv
    -- now we're ready to process the elems contained within the file, i.e imports, mod defs, etc. using the local env
    fileData' <- DF.foldlM evalTopElems fileData fileElems
    -- have finished the file, so update the global modenv using the modified fileData'
    liftIO $ debugM "ode3.modules" $ printf "Finished processing %s" (show modRoot)
    St.modSysState St.vModEnv (\modEnv -> Map.insert modRoot fileData' modEnv)

-- Actually loads an individual file of modules from a module root, return a list of top elems defined in the file
-- file has not been processed yet, simply parsed and desugared into Core AST
loadModFile :: ModRoot -> St.SysExceptIO [OdeTopElem DesId]
loadModFile modRoot = do
    mFileExist <- repoFileSearch
    case mFileExist of
        Nothing -> throwError $ printf "File %s not found in any module repositories" (show filePath)
                --debugM "ode3.modules" $ "File " ++ show filePath ++ " not found in any module repositories"
        Just modFilePath -> do
            -- need to load module - pass the data to orig mod parser
            liftIO $ debugM "ode3.modules" $ "Modules may be found in file " ++ modFilePath
            -- load the contents of the file
            modFileData <- liftIO $ readFile modFilePath
            -- st' :: MExcept SysState
            -- have to explicty case from Error to ErrorT monad
            -- actually parse the file using the module parser - return a list of the top elems
            St.liftExSys $ fileParse modFilePath modFileData modRoot
  where
    -- search for filePath exists roots
    repoFileSearch :: St.SysExceptIO (Maybe FilePath)
    repoFileSearch = do
        repos <- St.getSysState St.vRepos
        liftIO $ DF.msum <$> (mapM checkFile (OrdSet.toList repos))

    checkFile :: FilePath -> IO (Maybe FilePath)
    checkFile repo = Cond.ifM (Dir.doesFileExist repoFilePath) (return $ Just repoFilePath) (return $ Nothing)
      where
        repoFilePath = repo FP.</> filePath
    -- get FP
    filePath = uriToFilePath modRoot


-- | Takes a string representing the module URI and returns the path and module name
-- i.e. W.X.Y.Z -> (W/X/Y, Z)
uriToFilePath :: ModRoot -> FilePath
uriToFilePath modRoot = (FP.makeValid . FP.normalise . FP.joinPath $ modElems) FP.<.> "od3"
  where
    modElems = ListSplit.splitOn "." (getModRootStr modRoot)
