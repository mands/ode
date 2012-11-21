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

module Subsystem.ShellUI (
shellEntry
) where

import Control.Monad.Error
import Control.Monad.Trans(liftIO)
import qualified Control.Monad.State as S
import System.Environment(getArgs)
import System.Log.Logger

import qualified System.IO as SIO
import qualified System.Posix.Files as PF
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.FilePath as FP
import qualified System.Directory as Dir

import Data.Char (toLower)

-- fclabels stuff
import Control.Category
import Data.Label as L
import Prelude hiding ((.), id)

import Text.Show.Pretty

-- Shell backend - bit tricky
-- haskeline broken atm within latests HP - uses old mtl & transformers libs
-- editline doesn't build/interpret as missing symbol `el_reset`
-- readline works when compiled, but crashes in ghci, need to use basicBackend in that case
import System.Console.Shell
import System.Console.Shell.ShellMonad
-- import System.Console.Shell.Backend.Haskeline
-- import System.Console.Shell.Backend.Editline
import System.Console.Shell.Backend.Readline
import Utils.ShellHandleBackend

-- Ode Imports
import Utils.CommonImports
import Subsystem.SysState
import qualified Utils.OrdSet as OrdSet
import qualified Parser.Module as MP
import qualified AST.Module as MA
import qualified Subsystem.ModDriver as MD
import Process.Flatten (flatten)
import Subsystem.Simulation.Interpreter
import Subsystem.Simulation.JITCompiler


shellEntry :: IO ()
shellEntry = do
    argsLen <- liftM length getArgs

    -- setup IO based actions here
    initSysState <- mkDefSysState

    if argsLen == 0
      then do
        debugM "ode3.shell" "Using Featured Shell Backend"
        st' <- runShell (initShellDesc) (readlineBackend) initSysState
        -- putStrLn $ show st'
        return ()
      else do
        debugM "ode3.shell" "Using Basic Handle Backend"
        inName <- liftM head getArgs
        inHnd <- openPipe inName

        st' <- runShell (initShellDesc) (basicBackend inHnd) initSysState
        -- putStrLn $ show st'
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
-- TODO - change the toggle cmds to noun - not disableNoun
defaultCmds :: [ShellCommand SysState]
defaultCmds =   [ helpCommand "help" , showCmd, clearCmd, debugCmd, disableUnitsCmd
                , simStartCmd
                , startTimeCmd, stopTimeCmd, simTimestepCmd, simSolverCmd
                , simBackendCmd, simLinkerCmd, simExecuteCmd
                , simMathModelCmd, simMathLibCmd, simVecMathCmd
                , simOptimiseCmd, simShortCircuitCmd, simPowerExpanCmd
                , outPeriodCmd, outFilenameCmd
                , repoAddCmd, repoDelCmd
                , typeCmd
                , exitCommand "quit"
                ]
  where
    -- debug toggle, need to update the logger too
    debugCmd = toggle "debug" "Toggle Debug Mode" (get lDebug) (set lDebug)

    disableUnitsCmd = toggle "disableUnits" "Toggle Units Checking" (get lUnitsCheck) (set lUnitsCheck)

    -- basic cmds
    -- damn record update syntax!
    startTimeCmd = cmd "startTime" f "Initial simulation time"
      where
        f :: Double -> Sh SysState ()
        f x = modifyShellSt $ set (lStartTime . lSimParams) x

    stopTimeCmd = cmd "endTime" f "Final simulation time"
      where
        f :: Double -> Sh SysState ()
        f x = modifyShellSt $ set (lEndTime . lSimParams) x

    simTimestepCmd = cmd "timestep" f "Timestep to use for simulation"
      where
        f :: Double -> Sh SysState ()
        f x = modifyShellSt $ set (lTimestep . lSimParams) x

    simSolverCmd = cmd "solver" f "ODE Solver to use for simulation <euler, rk4>"
      where
        f :: String -> Sh SysState ()
        f str | map toLower str == "euler"   = modifyShellSt $ set (lSolver . lSimParams) FEuler
        f str | map toLower str == "rk4"     = modifyShellSt $ set (lSolver . lSimParams) RK4
        f _ = shellPutInfoLn "Possible options <euler, rk4>"

    simBackendCmd = cmd "backend" f "Simulation backend to use <interpreter, jitcompiler, aotcompiler>"
      where
        f :: String -> Sh SysState ()
        f str | map toLower str == "interpreter"     = modifyShellSt $ set (lBackend . lSimParams) Interpreter
        f str | map toLower str == "jitcompiler"     = modifyShellSt $ set (lBackend . lSimParams) JITCompiler
        f str | map toLower str == "aotcompiler"     = modifyShellSt $ set (lBackend . lSimParams) AOTCompiler
        f str | map toLower str == "objectfile"      = modifyShellSt $ set (lBackend . lSimParams) ObjectFile
        f str | map toLower str == "cvode"           = modifyShellSt $ set (lBackend . lSimParams) CVODE
        f _ = shellPutInfoLn "Possible options <interpreter, jitcompiler, aotcompiler>"

    simLinkerCmd = cmd "linker" f "System linker to use when compiling <dynamic, static>"
      where
        f :: String -> Sh SysState ()
        f str | map toLower str == "static"  = modifyShellSt $ set (lLinker . lSimParams) Static
        f str | map toLower str == "dynamic" = modifyShellSt $ set (lLinker . lSimParams) Dynamic
        f _ = shellPutInfoLn "Possible options <dynamic, static>"

    -- Sim Params Toggles
    simExecuteCmd       = toggle "disableExecute" "Toggle Execution of Simulations" (get $ lExecute . lSimParams) (set $ lExecute . lSimParams)
    simOptimiseCmd      = toggle "disableOptimise" "Toggle LLVM Optimisation of Simulations" (get $ lOptimise . lSimParams) (set $ lOptimise . lSimParams)
    simShortCircuitCmd  = toggle "disableShortCircuit" "Toggle Short-circuiting of boolean operators (N.B. may change simulation semantics) " (get $ lOptShortCircuit . lSimParams) (set $ lOptShortCircuit . lSimParams)
    simPowerExpanCmd    = toggle "disablePowerExpan" "Toggle Expansion of pow() calls (requires mathModel = fast)" (get $ lOptPowerExpan . lSimParams) (set $ lOptPowerExpan . lSimParams)
    simVecMathCmd       = toggle "vecMath" "Toggle Vectorisation Optimisation" (get $ lVecMath . lSimParams) (set $ lVecMath . lSimParams)

    simMathModelCmd = cmd "mathModel" f "Compilation Math model to utilise <strict, fast>"
      where
        f :: String -> Sh SysState ()
        f str | map toLower str == "strict"      = modifyShellSt $ set (lMathModel . lSimParams) Strict
        f str | map toLower str == "fast"        = modifyShellSt $ set (lMathModel . lSimParams) Fast
        f _ = shellPutInfoLn "Possible options <strict, fast>"

    simMathLibCmd = cmd "mathLib" f "Compilation Math Lib to utilise <gnu, amd, intel>"
      where
        f :: String -> Sh SysState ()
        f str | map toLower str == "gnu"      = modifyShellSt $ set (lMathLib . lSimParams) GNU
        f str | map toLower str == "amd"      = modifyShellSt $ set (lMathLib . lSimParams) AMD
        f str | map toLower str == "intel"    = modifyShellSt $ set (lMathLib . lSimParams) Intel
        f _ = shellPutInfoLn "Possible options <gnu, amd, intel>"

    outPeriodCmd = cmd "period" f "Period iterations to save simulation state to disk"
      where
        f :: Integer -> Sh SysState ()
        f x = modifyShellSt $ set (lOutputPeriod . lSimParams) x

    outFilenameCmd = cmd "output" f "Filename to save simulation results"
      where
        f :: File -> Sh SysState ()
        f (File x) = modifyShellSt $ set (lFilename . lSimParams) x

    simStartCmd = cmd "simulate" f "Start a simulation"
      where
        f :: String -> Sh SysState ()
        f initMod = shSimulate initMod

-- Repo Management -----------------------------------------------------------------------------------------------------


    repoAddCmd = cmd "addRepo" f "Add a directory path to the module repository (note - this clears all loaded modules)"
      where
        f :: File -> Sh SysState ()
        f (File repoPath) = do
            modifyShellSt clearModState
            st <- getShellSt
            dirEx <- liftIO $ Dir.doesDirectoryExist repoPath
            if dirEx
                then do
                    _ <- modifyShellSt $ modify vRepos (OrdSet.insertF repoPath)
                    shellPutInfoLn $ "Added " ++ repoPath ++ " to set of module repositories"
                else shellPutInfoLn $ "Module repository dir " ++ repoPath ++ " not found"

    repoDelCmd = cmd "delRepo" f "Delete a directory path from the module repository (note - this clears all loaded modules)"
      where
        f :: File -> Sh SysState ()
        f (File repoPath) = modifyShellSt $ (\st -> clearModState st |> modify vRepos (OrdSet.delete repoPath))

-- REPL Management -----------------------------------------------------------------------------------------------------

    -- show takes second string parameter
    showCmd = cmd "show" f "Pass <all, repos, modules> to display current state"
      where
        f :: String -> Sh SysState ()
        f "all" = (ppShow <$> getShellSt) >>= shellPutInfoLn
        f "debug" = f "simParams" >> f "modState"

        -- top level
        f "modState" =  (ppShow <$> get lModState <$> getShellSt) >>= shellPutInfoLn
        f "unitsState" = (ppShow <$> get lUnitsState <$> getShellSt) >>= shellPutInfoLn
        f "simParams" = (ppShow <$> get lSimParams <$> getShellSt) >>= shellPutInfoLn

        -- indiv useful elems
        f "repos" = (ppShow <$> get vRepos <$> getShellSt) >>= shellPutInfoLn
        f "modules" = (ppShow <$> get (lReplFile . lModState) <$> getShellSt) >>= shellPutInfoLn
        f _ = shellPutInfoLn "Pass <all, repos, modules, units> to display current state"

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
        Right (_, st') -> putShellSt st' -- >> shellPutInfoLn "Command complete"
    -- return, setting up new LOOP
    return ()
  where
    eval' :: SysExceptIO ()
    eval' = do
        -- READ cmd, pass the string to our mod lang parser
        mCmd <- lift $ MP.consoleParse str
        -- then EVAL, cmd sent to interpreter with state
        case mCmd of
            Just cmd -> do
                fd' <- join $ MD.evalTopElems <$> getSysState vLocalFile <*> pure cmd
                -- return the modified state (with the newly updated local filedata)
                putSysState vLocalFile fd'
            Nothing -> return ()


-- | Main simulation driver function
-- pulls up the initMod, converts into CoreFlat, (runs optimisations),
-- finally starts the interpreter/CPU/GPU backend with the Sim Params
shSimulate :: String -> Sh SysState ()
shSimulate initMod = do
    shellPutInfoLn $ printf "Simulation Params"
    (ppShow <$> get lSimParams <$> getShellSt) >>= shellPutInfoLn
    shellPutInfoLn $ printf "Starting simulation for module %s" (show initMod)

    st <- getShellSt
    eSt <- liftIO $ runErrorT (runSysExceptIO simulate' st)
    case eSt of
        Left err -> shellPutErrLn err
        -- update state and PRINT res
        Right (_, st') -> putShellSt st' >> shellPutInfoLn "Simulation complete"
    -- return, setting up new LOOP
    return ()

  where
    simulate' :: SysExceptIO ()
    simulate' = do
        -- now call the flatten function
        flatAST <- mkSysExceptIO $ flatten initMod
        -- optimise flatAST
        -- flatAST' <- optimise flatAST

        -- choose the correct backend
        backend <- L.get (lBackend . lSimParams) <$> S.get
        case backend of
            Interpreter -> interpret flatAST
            _ -> compileAndSimulate flatAST
        -- all done
        return ()
