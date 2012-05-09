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

import Control.Cond
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

data ModName = ModSing String | ModAll deriving Show


-- | Takes a string and returns the modname
loadModName :: String -> ModName
loadModName "*" = ModAll
loadModName x = ModSing x

-- | Takes a string representing the module URI and returns the path and module name
-- i.e. W.X.Y.Z -> (W/X/Y, Z)
uriToPath :: ModURIElems -> (FilePath, ModName)
uriToPath modElems = (uriFilePath, uriModName)
  where
    -- uriElems = ListSplit.splitOn "." uri
    uriModName = loadModName $ List.last modElems
    uriFilePath = (FP.makeValid . FP.normalise . FP.joinPath . List.init $ modElems) FP.<.> "od3"

-- | Takes a ModURI and return the original canonical mod name
canonicalModName :: ModURIElems -> ModURI
canonicalModName modElems = List.intercalate "." modElems

-- dummy func to load a module from file cmd
loadMod modElems = undefined
  where
    -- TODO - doesn't handle wildcard yet
    canonName = canonicalModName modElems
    (modFile, modName) = uriToPath modElems

    -- now try check repos


-- main REPL interpreter, maybe hook up to moduleDriver interpreter
--
interpretModCmd :: ModCmd -> ShState -> (MExcept ShState)
interpretModCmd (ModImport modElems Nothing) st = do
    -- get canon name
    let canonName = canonicalModName modElems
    -- get FP
    let (filePath, modName) = uriToPath modElems

    case (List.last modElems) of
        "*" -> do
            -- see if canon name is wildcard then check if both already loaded via FP and canon name in modEnv
            -- load everything
            if Set.member filePath (stParsedFiles st)
                then debugM "ode3.modules" ("Modules in " ++ filePath ++ " already loaded, ignoring") >> return (Right st)
                else do
                    debugM "ode3.modules" ("Modules in " ++ filePath ++ " not found, loading")
                    res <- loadModFile modElems filePath canonName Nothing st
                    -- update cache
                    return $ res >>= (\st -> Right $ st { stParsedFiles = Set.insert filePath (stParsedFiles st) })
        x -> do
            -- see if canon name is already in modEnv
            -- just load individual module (actually for now we load everything anyway)
            if Map.member canonName (stModuleEnv st)
                then debugM "ode3.modules" ("Module " ++ canonName ++ " already loaded into modEnv, ignoring") >> return (Right st)
                else do
                    debugM "ode3.modules" ("Module " ++ canonName ++ " not found in modEnv, loading")
                    res <- loadModFile modElems filePath canonName (Just x) st
                    -- update cache
                    -- HACK - we assume that we loaded all modules for a file for now and update cache accordingly
                    return $ res >>= (\st -> Right $ st { stParsedFiles = Set.insert filePath (stParsedFiles st) })


    -- if not for both tests, then load the module, else ifnore/print status message

    -- update the caches/state
    -- return


interpretModCmd (ModImport modElems (Just alias)) st = do
    -- import the module first
    interpretModCmd (ModImport modElems Nothing) st
    -- setup the alias
    interpretModCmd (ModAlias alias alias) st
    return $ Right st

-- make x an alias of y
interpretModCmd (ModAlias x y) st =
    -- check aliased module exists, if so then update modEnv
    case Map.member y modEnv of
        True -> return $ Right st { stModuleEnv = mkAlias }
        False -> return $ Left (show y ++ " not found in current module environment")
  where
    modEnv = stModuleEnv st
    -- insert an alias from X to Y in the modEnv, by creating a new VarMod pointer
    mkAlias = Map.insert x (VarMod y) modEnv



-- take everything for now
loadModFile :: ModURIElems -> FilePath -> ModURI -> Maybe String -> ShState -> IO (MExcept ShState)
loadModFile modElems filePath canonName modName st = do
    mFileExist <- repoFileSearch
    case mFileExist of
        Nothing -> do
            --debugM "ode3.modules" $ "File " ++ show filePath ++ " not found in any module repositories"
            return (throwError $ "File " ++ show filePath ++ " not found in any module repositories")
        Just modFilePath -> do
            -- need to load module - pass the data to orig mod parser
            debugM "ode3.modules" $ "Module found in " ++ modFilePath
            modFileData <- readFile modFilePath
            -- st' :: MExcept ShState
            let st' = modParse modFilePath modFileData canonRoot (stModuleEnv st)
                    >>= (\modEnv -> return $ st { stModuleEnv = modEnv })
            return st'
  where
    -- get the root canonName
    canonRoot = List.intercalate "." $ List.init modElems

    -- search for filePath exists roots
    repoFileSearch :: IO (Maybe FilePath)
    repoFileSearch = liftM DF.msum $ mapM checkFile (OrdSet.toList $ stRepos st)

    checkFile :: FilePath -> IO (Maybe FilePath)
    checkFile repo = ifM (Dir.doesFileExist repoFilePath) (return $ Just repoFilePath) (return $ Nothing)
      where
        repoFilePath = repo FP.</> filePath


