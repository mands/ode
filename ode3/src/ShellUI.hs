-----------------------------------------------------------------------------
--
-- Module      :  ShellUI
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Main functions that control the Shell interface
--
-----------------------------------------------------------------------------

module ShellUI (
shellEntry
) where

import qualified System.IO as SIO
import qualified System.Posix.Files as PF
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Control.Monad
import Control.Applicative

import System.Console.Shell
import System.Console.Shell.ShellMonad
import Utils.ShellHandleBackend
import System.Console.Shell.Backend.Readline

import System.Environment(getArgs)
import System.Log.Logger
import Utils.Utils

import qualified System.FilePath as FP
import qualified Data.List.Split as ListSplit

shellEntry = do
    argsLen <- liftM length getArgs

    if argsLen == 0
      then do
        debugM "ode3.shellui" "Using Readline"
        st' <- runShell (initShellDesc) (readlineBackend) initShState
        putStrLn $ show st'
      else do
        debugM "ode3.shellui" "Using Handle Backend"
        inName <- liftM head getArgs
        inHnd <- openPipe inName

        st' <- runShell (initShellDesc) (basicBackend inHnd) initShState
        putStrLn $ show st'
        closePipe inHnd

    putStrLn "Bye!"

    -- s <- getLine
    -- putStrLn $ "In - " ++ s
    -- hCmdPipe <- openPipe
    -- readLoop

    -- hGetContents inHnd >>= (\s -> putStrLn s)

    -- runShell (mkShellDescription [] react) (basicBackend inHnd)


-- make shell monad instnace of applicative
instance Applicative (Sh st) where
    pure = return
    (<*>) = ap


initShellDesc :: ShellDescription ShState
initShellDesc = desc'
  where
    desc = initialShellDescription
    -- update the initial desc
    desc' = desc {
        greetingText = Just "Welcome to Ode\n",
        prompt = \_ -> return ">> ",
        secondaryPrompt = Just $ \_ -> return ".> ",
        shellCommands = defaultCmds,
        evaluateFunc = shEval
     }

-- | Default commands used by the system, both toggles and cmd funcs
-- we use commands to setup and control simulation
defaultCmds :: [ShellCommand ShState]
defaultCmds =   [ helpCommand "help" , showCmd, clearCmd, debugCmd
                , simStartCmd
                , startTimeCmd, stopTimeCmd, simTimestepCmd
                , outPeriodCmd, outFilenameCmd
                , repoAddCmd, repoDelCmd
                , typeCmd
                , exitCommand "quit"
                ]
  where
    -- debug toggle
    debugCmd = toggle "debug" "Toggle Debug Mode" (\st -> stDebug st) (\b st -> st {stDebug = b} )

    -- basic cmds
    -- damn record update syntax!
    startTimeCmd = cmd "startTime" f "Initial simulation time"
      where
        f :: Float -> Sh ShState ()
        f x = modifyShellSt (\st -> st {stSimStart = x})

    stopTimeCmd = cmd "endTime" f "Final simulation time"
      where
        f :: Float -> Sh ShState ()
        f x = modifyShellSt (\st -> st {stSimEnd = x})

    simTimestepCmd = cmd "timestep" f "Timestep to use for simulation"
      where
        f :: Float -> Sh ShState ()
        f x = modifyShellSt (\st -> st {stSimTimestep = x})

    simStartCmd = cmd "start" f "Start a simulation"
      where
        f :: Sh ShState ()
        f = shellPutInfoLn "Starting simulation..."

    outPeriodCmd = cmd "period" f "Period iterations to save simulation state to disk"
      where
        f :: Integer -> Sh ShState ()
        f x = modifyShellSt (\st -> st {stOutPeriod = x})

    outFilenameCmd = cmd "output" f "Filename to save simulation results"
      where
        f :: File -> Sh ShState ()
        f (File x) = modifyShellSt (\st -> st {stOutFilename = x})

    repoAddCmd = cmd "addRepo" f "Add a directory path to the module repository"
      where
        f :: File -> Sh ShState ()
        f (File x) = modifyShellSt (\st -> st {stRepos = Set.insert x (stRepos st)})

    repoDelCmd = cmd "delRepo" f "Delete a directory path from the module repository"
      where
        f :: File -> Sh ShState ()
        f (File x) = modifyShellSt (\st -> st {stRepos = Set.delete x (stRepos st)})


    -- show takes second string parameter
    showCmd = cmd "show" f "Pass <all, repos, modules> to display current state"
      where
        f :: String -> Sh ShState ()
        f "all" = (show <$> getShellSt) >>= shellPutInfoLn
        f "repos" = (show <$> stRepos <$> getShellSt) >>= shellPutInfoLn
        f "modules" = (show <$> stModules <$> getShellSt) >>= shellPutInfoLn
        f _ = shellPutInfoLn "Pass <all, repos, modules> to display current state"

    typeCmd = cmd "type" f "Display the type of the loaded module"
      where
        f :: String -> Sh ShState ()
        f x = shellPutErrLn "Not yet implemented"

    clearCmd = cmd "clear" f "Remove all modules, reset system to defaults"
      where
        -- TODO - need to actually unload data from system
        f :: Sh ShState ()
        f = putShellSt initShState

-- | Main shell eval function, takes the input string and passes to the parsec parser responsible for
-- we use eval function for the run-time language, i.e. loading, applying and creating models for simulation
shEval :: String -> Sh ShState ()
shEval str = shellPutInfoLn str


-- | Main system state used by the shell
data ShState = ShState  { stDebug :: Bool               -- do we enable debug mode
                        , stSimStart :: Float           -- simulation params
                        , stSimEnd :: Float
                        , stSimTimestep :: Float        -- simulation timestep
                        , stOutPeriod     :: Integer    -- period with which to save simulation state to outfile, wrt timestep
                        , stOutFilename :: FilePath     -- output filename to save data to
                        , stRepos :: Set.Set FilePath       -- list of enabled module repositories
                        , stModules :: Map.Map String Bool   -- map of loaded modules

                        -- what else??
                        } deriving Show

-- | Sensible default values for initial system state
initShState = ShState   { stDebug = False
                        , stSimStart = 0
                        , stSimEnd = 60
                        , stSimTimestep = 0.001         -- 1ms
                        , stOutPeriod = 500             -- 0.5s
                        , stOutFilename = "output.bin"  -- default output file
                        , stRepos = Set.empty           -- do we add the defaults here?
                        , stModules = Map.empty
                        }

-- some module helper funcs, need to relocate
data ModImport = ModImport FilePath ModName (Maybe String) deriving Show
data ModName = ModSing FilePath | ModAll deriving Show

-- | Takes a string and returns the modname
loadModName :: String -> ModName
loadModName "*" = ModAll
loadModName x = ModSing x

-- | Takes a string representing the module URI and returns the path and module name
-- i.e. W.X.Y.Z -> (W/X/Y, Z)
uriToPath :: String -> (FilePath, ModName)
uriToPath uri = (uriFilePath, uriModName)
  where
    uriElems = ListSplit.splitOn "." uri
    uriModName = loadModName $ List.last uriElems

    uriFilePath = (FP.makeValid . FP.normalise . FP.joinPath . List.init $ uriElems) FP.<.> "od3"




