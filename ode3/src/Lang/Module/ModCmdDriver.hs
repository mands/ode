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
evalTopElems :: ShState -> OdeTopElem E.DesId -> MExceptIO ShState
evalTopElems st topMod@(TopMod modRoot modName mod) = do
    modEnv' <- mkExceptIO $ Map.insert <$> pure modName <*> eRes <*> pure modEnv
    return $ st { stLocalModEnv = modEnv' }
  where
    modEnv = stLocalModEnv st
    eRes :: MExcept (Module E.Id)
    eRes = checkName *> evalModDef modEnv mod
    -- check if module already exists
    checkName = if Map.member modName modEnv then throwError ("(MD06) - Module with name " ++ (show modName) ++ " already defined") else pure ()
    fullModName = mkModFullName (Just modRoot) modName


-- top import, all modules, called from REPL or within a file
evalTopElems st modCmd@(ModImport modRoot Nothing) =
    if Set.member modRoot parsedFiles -- if we've parsed this file already
        then if Map.member modRoot (stGlobalModEnv st) -- then it should be in global cache
            then do -- so add it from glboal to local, overwriting any clashes that may occur
                undefined


                -- liftIO $ debugM "ode3.modules" ("Importing modules in " ++ show modRoot ++ " from global to local env")
                -- let localEnv' = Map.union (fromJust . Map.lookup modRoot $ stGlobalModEnv st) (stLocalModEnv st)
                -- return $ st { stLocalModEnv = localEnv' }
            else -- ifnot, shit, error, we must be in the process of analysing the file already, is parsed but not global cache
                throwError $ "Modules " ++ show modRoot ++ " have already been parsed, import cycle detected"
        else do -- module not parsed, this should only occur from within REPL, preemptive importing should catch other cases in files
            liftIO $ debugM "ode3.modules" ("Searching for modules in " ++ show modRoot)
            st' <- processImport st modRoot
            -- now re-eval the cmd as we've loaded what we need to
            evalTopElems st' modCmd
  where
    -- parsed Files
    parsedFiles = stParsedFiles st


-- import cmd - only selected modules from file
evalTopElems st (ModImport modRoot (Just mods)) = trace' [MkSB modRoot, MkSB mods] "In import.just" $ undefined
  where
    -- set of modules to load from file
    modsLoad :: [ModName]
    modsLoad = do
        let modEnv = (stLocalModEnv st)
        (m, _) <- mods
        -- let modName = mkModFullName modRootElems m
        -- guard $ Map.notMember modName modEnv -- do we need this?
        return m
    -- list of modules to alias
    aliasCmds = [ ModAlias (fromJust a) m | (m, a) <- mods, isJust a == True ]
    -- parsed Files
    parsedFiles = stParsedFiles st

    old = do
        -- see if canon name is already in modEnv
        -- just load individual module (actually for now we load everything anyway)
        st' <- if null modsLoad
            then liftIO $ debugM "ode3.modules" ("Modules " ++ show (map fst mods) ++ " already loaded into modEnv, ignoring") >> return st
            else do
                liftIO $ debugM "ode3.modules" ("Modules " ++ show modsLoad ++ " not found in modEnv, loading")
                (fileElems, pSt) <- loadModFile modRoot st
                -- update cache
                -- HACK - we assume that we loaded all modules for a file for now and update cache accordingly
                let st' = st { stParsedFiles = Set.insert modRoot (stParsedFiles st') }

                -- do we recurse here??
                st'' <- DF.foldlM evalTopElems st' fileElems
                return $ st''

        -- setup the alias if needed
        st'' <- DF.foldlM evalTopElems st' aliasCmds

        return st''


-- | make aliasName an alias of origName
evalTopElems st (ModAlias aliasName origName) =
    -- check aliased module exists, if so then update modEnv
    case Map.member origName modEnv of
        True -> return $ st { stLocalModEnv = mkAlias }
        False -> throwError $ show origName ++ " not found in current module environment"
  where
    modEnv = stLocalModEnv st
    -- insert an alias from Y to X in the modEnv, by creating a new VarMod pointer
    mkAlias = Map.insert aliasName (VarMod (mkModFullName Nothing origName)) modEnv
    -- origName = mkModRoot origElems



-- | Takes a string representing the module URI and returns the path and module name
-- i.e. W.X.Y.Z -> (W/X/Y, Z)
uriToFilePath :: ModRoot -> FilePath
uriToFilePath (ModRoot modRoot) = (FP.makeValid . FP.normalise . FP.joinPath $ modElems) FP.<.> "od3"
  where
    modElems = ListSplit.splitOn "." modRoot

processImport :: ShState -> ModRoot -> MExceptIO ShState
processImport st modRoot = do
    -- load the file
    (fileElems, pSt) <- loadModFile modRoot st
    -- update parsed files cache
    let st' = st { stParsedFiles = Set.insert modRoot (stParsedFiles st) }
    -- pre-emptively load all the referenced imports into the global env
    st'' <- loadRefImports modRoot st' pSt
    -- process the file, having reset the local modEnv
    -- now we're ready to process the elems contained within the file, i.e imports, mod defs, etc. using the local env
    st''' <- DF.foldlM evalTopElems (st'' { stLocalModEnv = Map.empty }) fileElems
    -- have finished the file, so update the global modenv
    undefined


    -- return $ st''' { stLocalModEnv = Map.empty, stGlobalModEnv = Map.insert modRoot (stLocalModEnv st''') (stGlobalModEnv st''') }

-- Loads an individual module file, taking the filename, module root, list of modules to load and current state
loadModFile :: ModRoot -> ShState -> MExceptIO ([OdeTopElem E.DesId], CP.PState)
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
