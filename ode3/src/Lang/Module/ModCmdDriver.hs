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
        -- trace' [MkSB fd, MkSB mod] "updateState" $ return ()
        let fd' = fd { fileModEnv = Map.insert modName mod (fileModEnv fd) }
        -- only update global modEnv if not the console fileMod
        unless ((fileModRoot fd) == replModRoot) $
            St.modSysState St.vModEnv (\modEnv -> Map.insert modRoot fd' modEnv)
            -- update fEnv within global state after processing each module to allow refernces to modules in the same file
            -- using the full path
        return fd'

-- top import, called from REPL or within a file
evalTopElems fd (TopModImport importCmd@(ModImport modRoot mMods)) = do
    lEnv' <- evalImport (fileModEnv fd) importCmd
    return $ fd { fileModEnv = lEnv' }

-- Module Importing Evaluation -----------------------------------------------------------------------------------------

-- | Main function to eval an import command with respect to the system state
evalImport :: LocalModEnv -> ModImport -> St.SysExceptIO LocalModEnv
evalImport mEnv importCmd@(ModImport modRoot _) = do
    st <- S.get
    if Set.member modRoot (get St.vParsedFiles st) -- if we've parsed this file already
        then if Map.member modRoot (get St.vModEnv st) -- then it should be in global cache
            then -- so add the imports to the cur filedata
                addImportsToEnv mEnv importCmd
            else -- ifnot, shit, error, we must be in the process of analysing the file already, is parsed but not global cache
                throwError $ printf "Modules %s have already been parsed, import cycle detected" (show modRoot)
        else do -- module not parsed, this should can occur from within REPL, top file, or module
            -- liftIO $ debugM "ode3.modules" ("Searching for modules in " ++ show modRoot)
            -- need to load module
            loadImport modRoot
            -- now process the import cmd
            addImportsToEnv mEnv importCmd

-- | Imports the modules cmds into the given LocalModEnv
-- TODO - this doesn't need IO
addImportsToEnv :: LocalModEnv -> ModImport -> St.SysExceptIO LocalModEnv
addImportsToEnv mEnv (ModImport modRoot mMods) = do
    -- get the fileModEnv for the imported file
    gModEnv <- (St.getSysState St.vModEnv)
    fModEnv <- fileModEnv <$> (St.liftExSys $ getFileData modRoot gModEnv)
    -- update the cur modEnv with those from the imported modules
    DF.foldlM (addImport fModEnv) mEnv (calcImportedMods fModEnv)
  where
    -- create a list of modules to import, either all, or the given selection
    calcImportedMods fModEnv = maybe (map (\mod -> (mod, Nothing)) $ Map.keys fModEnv) id mMods
    -- create an import ref for the given modName, with potential alias
    addImport fModEnv mEnv (modName, mAlias) =
        case Map.lookup modName fModEnv of
            Just mod -> do
                let impModName = maybe modName id mAlias
                if Map.member impModName mEnv
                    then throwError $ printf "Module %s already defined/imported in module" (show impModName)
                    else do
                        -- create a RefMod here to stand in for the import
                        let refMod = mkRefMod (ModFullName modRoot modName) mod
                        trace' [MkSB refMod] "addImports" $ return $ Map.insert impModName refMod mEnv
            Nothing -> throwError $ printf "Imported module %s not found in %s" (show modName) (show modRoot)

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
