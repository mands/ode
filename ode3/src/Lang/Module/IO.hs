-----------------------------------------------------------------------------
--
-- Module      :  Lang.Module.IO
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Collection of data/functions to handle searching and loading module files within module repos
--
-----------------------------------------------------------------------------

module Lang.Module.IO (
interpretModCmd
) where

import Control.Monad
import Control.Monad.Error
import Data.Foldable as DF

import Control.Conditional
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.List as List
import qualified Data.List.Split as ListSplit
import qualified System.FilePath as FP
import qualified System.Directory as Dir
import System.Log.Logger

import UI.ShellState
import Lang.Module.AST
import Lang.Module.Parser

import Utils.OrdSet as OrdSet
import Utils.Utils

-- | Takes a string representing the module URI and returns the path and module name
-- i.e. W.X.Y.Z -> (W/X/Y, Z)
uriToPath :: ModURIElems -> FilePath
uriToPath modElems = (FP.makeValid . FP.normalise . FP.joinPath $ modElems) FP.<.> "od3"

indivModName :: ModURIElems -> ModURI
indivModName modElems = List.last modElems

-- | Takes a ModURI and return the original canonical mod name
canonicalModName :: ModURIElems -> ModURI
canonicalModName modElems = List.intercalate "." modElems

-- main REPL interpreter, maybe hook up to moduleDriver interpreter
--
interpretModCmd :: ModCmd -> ShState -> MExceptIO ShState
interpretModCmd (ModImportAll modElems) st =
    -- see if canon name is wildcard then check if both already loaded via FP and canon name in modEnv
    -- load everything
    if Set.member filePath (stParsedFiles st)
        then liftIO $ debugM "ode3.modules" ("Modules in " ++ filePath ++ " already loaded, ignoring") >> return st
        else do
            liftIO $ debugM "ode3.modules" ("Modules in " ++ filePath ++ " not found, loading")
            st' <- loadModFile filePath canonRoot st
            -- update cache
            return $ st' { stParsedFiles = Set.insert filePath (stParsedFiles st') }
  where
    -- get canon name for root
    canonRoot = canonicalModName modElems
    -- get FP
    filePath = uriToPath modElems

interpretModCmd (ModImport modElems mAlias) st = do
    -- see if canon name is already in modEnv
    -- just load individual module (actually for now we load everything anyway)
    st' <- if Map.member canonName (stModuleEnv st)
        then liftIO $ debugM "ode3.modules" ("Module " ++ canonName ++ " already loaded into modEnv, ignoring") >> return st
        else do
            liftIO $ debugM "ode3.modules" ("Module " ++ canonName ++ " not found in modEnv, loading")
            st' <- loadModFile filePath canonRoot st
            -- update cache
            -- HACK - we assume that we loaded all modules for a file for now and update cache accordingly
            return $ st' { stParsedFiles = Set.insert filePath (stParsedFiles st') }
    -- setup the alias if needed
    maybe (return st') (\alias -> interpretModCmd (ModAlias alias modElems) st') mAlias
  where
    -- get canon name
    canonName = canonicalModName modElems
    canonRoot = canonicalModName $ List.init modElems
    -- get FP
    filePath = uriToPath $ List.init modElems
    modName = indivModName modElems


-- | make aliasName an alias of origName
interpretModCmd (ModAlias aliasName origElems) st =
    -- check aliased module exists, if so then update modEnv
    case Map.member origName modEnv of
        True -> return $ st { stModuleEnv = mkAlias }
        False -> throwError $ show origName ++ " not found in current module environment"
  where
    modEnv = stModuleEnv st
    -- insert an alias from Y to X in the modEnv, by creating a new VarMod pointer
    mkAlias = Map.insert aliasName (VarMod origName) modEnv
    origName = canonicalModName origElems


-- take everything for now
loadModFile :: FilePath -> ModURI -> ShState -> MExceptIO ShState
loadModFile filePath canonRoot st = do
    mFileExist <- liftIO repoFileSearch
    case mFileExist of
        Nothing -> throwError $ "File " ++ show filePath ++ " not found in any module repositories"
                --debugM "ode3.modules" $ "File " ++ show filePath ++ " not found in any module repositories"
        Just modFilePath -> do
            -- need to load module - pass the data to orig mod parser
            liftIO $ debugM "ode3.modules" $ "Module found in " ++ modFilePath
            modFileData <- liftIO $ readFile modFilePath
            -- st' :: MExcept ShState
            -- have to explicty case from Error to ErrorT monad
            case modParse modFilePath modFileData canonRoot (stModuleEnv st) of
                Left e -> throwError e
                Right mod' -> return $ st { stModuleEnv = mod' }
  where
    -- get the root canonName
    --canonRoot = canonicalModName modElems

    -- search for filePath exists roots
    repoFileSearch :: IO (Maybe FilePath)
    repoFileSearch = liftM DF.msum $ mapM checkFile (OrdSet.toList $ stRepos st)

    checkFile :: FilePath -> IO (Maybe FilePath)
    checkFile repo = ifM (Dir.doesFileExist repoFilePath) (return $ Just repoFilePath) (return $ Nothing)
      where
        repoFilePath = repo FP.</> filePath


