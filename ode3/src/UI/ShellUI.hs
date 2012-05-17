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
shellEntry, ShState(..)
) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans(liftIO)
import Control.Applicative
import System.Environment(getArgs)
import System.Log.Logger

import qualified System.IO as SIO
import qualified System.Posix.Files as PF
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.FilePath as FP
import qualified System.Directory as Dir

import System.Console.Shell
import System.Console.Shell.ShellMonad
-- import System.Console.Shell.Backend.Readline
import System.Console.Shell.Backend.Haskeline
import Utils.ShellHandleBackend
import UI.ShellState

import Utils.Utils
import qualified Utils.OrdSet as OrdSet
import qualified Lang.Module.Parser as MP
import qualified Lang.Module.AST as MA
import qualified Lang.Module.ModuleDriver as MD


shellEntry :: IO ()
shellEntry = do
    argsLen <- liftM length getArgs

    -- setup IO based actions here
    initShState <- mkDefShState

    if argsLen == 0
      then do
        debugM "ode3.shell" "Using Featured Shell Backend"
        st' <- runShell (initShellDesc) (haskelineBackend) initShState
        putStrLn $ show st'
      else do
        debugM "ode3.shell" "Using Basic Handle Backend"
        inName <- liftM head getArgs
        inHnd <- openPipe inName

        st' <- runShell (initShellDesc) (basicBackend inHnd) initShState
        putStrLn $ show st'
        closePipe inHnd

    putStrLn "Bye!"


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
    -- debug toggle, need to update the logger too
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
        f (File repoPath) = do
            st <- getShellSt
            dirEx <- liftIO $ Dir.doesDirectoryExist repoPath
            if dirEx
                then do
                    let repos' = OrdSet.insertF repoPath (stRepos st)
                    _ <- modifyShellSt (\st -> st { stRepos = repos' })
                    shellPutInfoLn $ "Added " ++ repoPath ++ " to set of module repositories"
                else shellPutInfoLn $ "Module repository dir " ++ repoPath ++ " not found"


    repoDelCmd = cmd "delRepo" f "Delete a directory path from the module repository"
      where
        f :: File -> Sh ShState ()
        f (File repoPath) = modifyShellSt (\st -> st {stRepos = OrdSet.delete repoPath (stRepos st)})


    -- show takes second string parameter
    showCmd = cmd "show" f "Pass <all, repos, modules> to display current state"
      where
        f :: String -> Sh ShState ()
        f "all" = (show <$> getShellSt) >>= shellPutInfoLn
        f "repos" = (show <$> stRepos <$> getShellSt) >>= shellPutInfoLn
        f "modules" = (show <$> Map.keys <$> stLocalModEnv <$> getShellSt) >>= shellPutInfoLn
        f _ = shellPutInfoLn "Pass <all, repos, modules> to display current state"

    typeCmd = cmd "type" f "Display the type of the loaded module"
      where
        f :: String -> Sh ShState ()
        f x = shellPutErrLn "Not yet implemented"

    clearCmd = cmd "clear" f "Remove all modules, reset system to defaults"
      where
        -- TODO - need to actually unload data from system
        f :: Sh ShState ()
        f = liftIO mkDefShState >>= putShellSt

-- | Main shell eval function, takes the input string and passes to the parsec parser responsible for
-- we use eval function for the run-time language, i.e. loading, applying and creating models for simulation
-- main REPL goes here, READ-EVAL-PRINT-LOOP
shEval :: String -> Sh ShState ()
shEval str = do
    shellPutInfoLn str
    st <- getShellSt
    eSt <- liftIO $ runErrorT (eval' st)
    case eSt of
        Left err -> shellPutErrLn err
        -- update state and PRINT res
        Right st' -> putShellSt st' >> shellPutInfoLn "Command complete"
    -- return, setting up new LOOP
    return ()
  where
    eval' :: ShState -> MExceptIO ShState
    eval' st = do
        -- READ cmd, pass the string to our mod lang parser
        cmd <- MP.consoleParse str
        -- then EVAL, cmd sent to interpreter with state
        MD.evalTopElems st cmd