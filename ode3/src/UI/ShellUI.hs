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

module UI.ShellUI (
shellEntry
) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans(liftIO)
import qualified Control.Monad.State as S
import Control.Applicative
import System.Environment(getArgs)
import System.Log.Logger

import qualified System.IO as SIO
import qualified System.Posix.Files as PF
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.FilePath as FP
import qualified System.Directory as Dir

-- fclabels stuff
import Control.Category
import Data.Label
import Prelude hiding ((.), id)

import System.Console.Shell
import System.Console.Shell.ShellMonad
-- import System.Console.Shell.Backend.Haskeline
-- import System.Console.Shell.Backend.Editline
import System.Console.Shell.Backend.Readline
import Utils.ShellHandleBackend
import UI.SysState

import Utils.Utils
import qualified Utils.OrdSet as OrdSet
import qualified Lang.Module.Parser as MP
import qualified Lang.Module.AST as MA
import qualified Lang.Module.ModCmdDriver as MD


shellEntry :: IO ()
shellEntry = do
    argsLen <- liftM length getArgs

    -- setup IO based actions here
    initSysState <- mkDefSysState

    if argsLen == 0
      then do
        debugM "ode3.shell" "Using Featured Shell Backend"
        st' <- runShell (initShellDesc) (readlineBackend) initSysState
        putStrLn $ show st'
      else do
        debugM "ode3.shell" "Using Basic Handle Backend"
        inName <- liftM head getArgs
        inHnd <- openPipe inName

        st' <- runShell (initShellDesc) (basicBackend inHnd) initSysState
        putStrLn $ show st'
        closePipe inHnd

    putStrLn "Bye!"


-- make shell monad instnace of applicative
instance Applicative (Sh st) where
    pure = return
    (<*>) = ap


initShellDesc :: ShellDescription SysState
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
defaultCmds :: [ShellCommand SysState]
defaultCmds =   [ helpCommand "help" , showCmd, clearCmd, debugCmd
                , simStartCmd
                , startTimeCmd, stopTimeCmd, simTimestepCmd
                , outPeriodCmd, outFilenameCmd
                , repoAddCmd, repoDelCmd
                , typeCmd
                , exitCommand "quit"
                ]
  where
    -- debug toggle, need to update the logger too
    debugCmd = toggle "debug" "Toggle Debug Mode" (get lDebug) (set lDebug)

    -- basic cmds
    -- damn record update syntax!
    startTimeCmd = cmd "startTime" f "Initial simulation time"
      where
        f :: Float -> Sh SysState ()
        f x = modifyShellSt $ set (lStartTime . lSimState) x

    stopTimeCmd = cmd "endTime" f "Final simulation time"
      where
        f :: Float -> Sh SysState ()
        f x = modifyShellSt $ set (lEndTime . lSimState) x

    simTimestepCmd = cmd "timestep" f "Timestep to use for simulation"
      where
        f :: Float -> Sh SysState ()
        f x = modifyShellSt $ set (lTimestep . lSimState) x

    simStartCmd = cmd "start" f "Start a simulation"
      where
        f :: Sh SysState ()
        f = shellPutInfoLn "Starting simulation..."

    outPeriodCmd = cmd "period" f "Period iterations to save simulation state to disk"
      where
        f :: Integer -> Sh SysState ()
        f x = modifyShellSt $ set (lOutputPeriod . lSimState) x

    outFilenameCmd = cmd "output" f "Filename to save simulation results"
      where
        f :: File -> Sh SysState ()
        f (File x) = modifyShellSt $ set (lFilename . lSimState) x

    repoAddCmd = cmd "addRepo" f "Add a directory path to the module repository"
      where
        f :: File -> Sh SysState ()
        f (File repoPath) = do
            st <- getShellSt
            dirEx <- liftIO $ Dir.doesDirectoryExist repoPath
            if dirEx
                then do
                    _ <- modifyShellSt $ modify vRepos (OrdSet.insertF repoPath)
                    shellPutInfoLn $ "Added " ++ repoPath ++ " to set of module repositories"
                else shellPutInfoLn $ "Module repository dir " ++ repoPath ++ " not found"


    repoDelCmd = cmd "delRepo" f "Delete a directory path from the module repository"
      where
        f :: File -> Sh SysState ()
        f (File repoPath) = modifyShellSt $ modify vRepos (OrdSet.delete repoPath)
    -- show takes second string parameter
    showCmd = cmd "show" f "Pass <all, repos, modules> to display current state"
      where
        f :: String -> Sh SysState ()
        f "all" = (show <$> getShellSt) >>= shellPutInfoLn
        f "repos" = (show <$> get vRepos <$> getShellSt) >>= shellPutInfoLn
        f "modules" = (show <$> get (lLocalFile . lModState) <$> getShellSt) >>= shellPutInfoLn
        f _ = shellPutInfoLn "Pass <all, repos, modules> to display current state"

    typeCmd = cmd "type" f "Display the type of the loaded module"
      where
        f :: String -> Sh SysState ()
        f x = shellPutErrLn "Not yet implemented"

    clearCmd = cmd "clear" f "Remove all modules, reset system to defaults"
      where
        -- TODO - need to actually unload data from system
        f :: Sh SysState ()
        f = liftIO mkDefSysState >>= putShellSt

-- | Main shell eval function, takes the input string and passes to the parsec parser responsible for
-- we use eval function for the run-time language, i.e. loading, applying and creating models for simulation
-- main REPL goes here, READ-EVAL-PRINT-LOOP
shEval :: String -> Sh SysState ()
shEval str = do
    shellPutInfoLn str
    st <- getShellSt
    eSt <- liftIO $ runErrorT (runSysExceptIO eval' st)
    case eSt of
        Left err -> shellPutErrLn err
        -- update state and PRINT res
        Right (_, st') -> putShellSt st' >> shellPutInfoLn "Command complete"
    -- return, setting up new LOOP
    return ()
  where
    eval' :: SysExceptIO ()
    eval' = do
        -- READ cmd, pass the string to our mod lang parser
        let cmd = lift $ MP.consoleParse str
        -- then EVAL, cmd sent to interpreter with state
        fd' <- join $ MD.evalTopElems <$> getSysState vLocalFile <*> cmd
        -- return the modified state (with the newly updated local filedata)
        putSysState vLocalFile fd'
