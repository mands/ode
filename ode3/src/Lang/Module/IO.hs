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

import qualified Control.Conditional as Cond
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Maybe(isJust, fromJust)
import qualified Data.List as List
import qualified Data.List.Split as ListSplit
import qualified System.FilePath as FP
import qualified System.Directory as Dir
import System.Log.Logger

import UI.ShellState
import Lang.Module.AST
import Lang.Module.Parser

import qualified Utils.OrdSet as OrdSet
import Utils.Utils

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

-- main REPL interpreter, maybe hook up to moduleDriver interpreter
--
interpretModCmd :: ModCmd -> ShState -> MExceptIO ShState
interpretModCmd (ModImport modRootElems Nothing) st =
    -- import all modules from file
    -- check if file already loaded via FP and canon name in modEnv
    if Set.member filePath (stParsedFiles st)
        then liftIO $ debugM "ode3.modules" ("Modules in " ++ filePath ++ " already loaded, ignoring") >> return st
        else do
            liftIO $ debugM "ode3.modules" ("Modules in " ++ filePath ++ " not found, loading")
            st' <- loadModFile filePath canonRoot Nothing st
            -- update cache
            return $ st' { stParsedFiles = Set.insert filePath (stParsedFiles st') }
  where
    -- get canon name for root
    canonRoot = flattenURI modRootElems
    -- get FP
    filePath = uriToPath modRootElems


interpretModCmd (ModImport modRootElems (Just mods)) st = do
    -- see if canon name is already in modEnv
    -- just load individual module (actually for now we load everything anyway)
    st' <- if null modsLoad
        then liftIO $ debugM "ode3.modules" ("Modules " ++ show (map fst mods) ++ " already loaded into modEnv, ignoring") >> return st
        else do
            liftIO $ debugM "ode3.modules" ("Modules " ++ show modsLoad ++ " not found in modEnv, loading")
            st' <- loadModFile filePath canonRoot (Just modsLoad) st
            -- update cache
            -- HACK - we assume that we loaded all modules for a file for now and update cache accordingly
            return $ st' { stParsedFiles = Set.insert filePath (stParsedFiles st') }

    -- setup the alias if needed
    st'' <- foldlM (\st alias -> interpretModCmd alias st) st' aliasCmds

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
    origName = flattenURI origElems


-- Loads an individual module file, taking the filename, module root, list of modules to load and current state
loadModFile :: FilePath -> ModURI -> Maybe [ModURI] -> ShState -> MExceptIO ShState
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
            case modParse modFilePath modFileData canonRoot (stModuleEnv st) of
                Left e -> throwError e
                Right mod' -> return $ st { stModuleEnv = mod' }
  where
    -- search for filePath exists roots
    repoFileSearch :: IO (Maybe FilePath)
    repoFileSearch = liftM DF.msum $ mapM checkFile (OrdSet.toList $ stRepos st)

    checkFile :: FilePath -> IO (Maybe FilePath)
    checkFile repo = Cond.ifM (Dir.doesFileExist repoFilePath) (return $ Just repoFilePath) (return $ Nothing)
      where
        repoFilePath = repo FP.</> filePath


