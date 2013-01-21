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
import qualified Subsystem.Units as U


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
defaultCmds =   [ helpCommand "help" , showCmd, clearCmd, debugCmd, simStartCmd, typeCmd, exitCommand "quit"
                , repoAddCmd, repoDelCmd
                -- sim params
                , simStartTimeCmd, simStopTimeCmd, simTimestepCmd
                , simMaxTimestepCmd, simMaxNumStepsCmd, simRelErrorCmd, simAbsErrorCmd, simModelType
                , simDisableUnitsCmd, simTimeUnitCmd, simOutPeriodCmd, simOutFilenameCmd
                , simOdeSolverCmd, simSdeSolverCmd, simBackendCmd, simExeFilenameCmd, simLinkerCmd, simExecuteCmd
                , simMathModelCmd, simMathLibCmd, simVecMathCmd
                , simOptimiseCmd, simShortCircuitCmd, simPowerExpanCmd
                ]
  where
    -- debug toggle, need to update the logger too
    debugCmd = toggle "debug" "Toggle Debug Mode" (get lDebug) (set lDebug)

    -- basic cmds
    -- damn record update syntax!
    -- helper func to set a type-safe double val
    setDouble :: (:->) SysState Double -> Double -> Sh SysState ()
    setDouble lbl val = modifyShellSt $ set lbl val

    -- basic params
    simStartTimeCmd = cmd "startTime" (setDouble $ lStartTime . lSimParams) "Initial simulation time"
    simStopTimeCmd = cmd "stopTime" (setDouble $ lStopTime . lSimParams) "Final simulation time"
    simTimestepCmd = cmd "timestep" (setDouble $ lTimestep . lSimParams) "Timestep to use for simulation"

    -- unit params
    simDisableUnitsCmd = toggle "disableUnits" "Toggle Units Checking" (get $ lUnitsCheck . lSimParams) (set $ lUnitsCheck . lSimParams)
    simTimeUnitCmd = cmd "timeUnit" f "Set the unit used for the independent time parameter (only s fully supported!)"
      where
        f :: String -> Sh SysState ()
        f str | map toLower str == "ns"  = modifyShellSt $ set (lTimeUnit . lSimParams) U.uNanoSeconds
        f str | map toLower str == "ms"  = modifyShellSt $ set (lTimeUnit . lSimParams) U.uMilliSeconds
        f str | map toLower str == "s"   = modifyShellSt $ set (lTimeUnit . lSimParams) U.uSeconds
        f str | map toLower str == "min" = modifyShellSt $ set (lTimeUnit . lSimParams) U.uMinutes
        f str | map toLower str == "hr"  = modifyShellSt $ set (lTimeUnit . lSimParams) U.uHours
        f _  = shellPutInfoLn "Possible options <ns, ms, s, min, hr> (only s fully supported!)"

    -- adaptive params
    simMaxTimestepCmd = cmd "maxTimestep" (setDouble $ lMaxTimestep . lSimParams) "Max timestep to use for simulation"
    simMaxNumStepsCmd = cmd "maxNumSteps" f "Max number of steps to use from current to next output time"
      where
        f :: Integer -> Sh SysState ()
        f x = modifyShellSt $ set (lMaxNumSteps . lSimParams) x
    simRelErrorCmd = cmd "relError" (setDouble $ lRelError . lSimParams) "Relative error for adaptive simulation"
    simAbsErrorCmd = cmd "absError" (setDouble $ lAbsError . lSimParams) "Absolute error for adaptive simulation"
    simModelType = cmd "modelType" f "Model Type to use for adaptive simulation <stiff, nonstiff>"
      where
        f :: String -> Sh SysState ()
        f str | map toLower str == "stiff"       = modifyShellSt $ set (lModelType . lSimParams) Stiff
        f str | map toLower str == "nonstiff"    = modifyShellSt $ set (lModelType . lSimParams) NonStiff
        f _ = shellPutInfoLn "Possible options <stiff, nonstiff>"

    -- output params
    simOutPeriodCmd = cmd "period" (setDouble $ lOutputPeriod . lSimParams) "Interval period to save simulation state to disk (seconds)"
    simOutFilenameCmd = cmd "output" f "Filename to save simulation results"
      where
        f :: File -> Sh SysState ()
        f (File x) = modifyShellSt $ set (lFilename . lSimParams) x

    -- compilation params
    simOdeSolverCmd = cmd "odeSolver" f "ODE Solver to use for simulation <euler, rk4, adaptive>"
      where
        f :: String -> Sh SysState ()
        f str | map toLower str == "euler"       = modifyShellSt $ set (lOdeSolver . lSimParams) FEuler
        f str | map toLower str == "rk4"         = modifyShellSt $ set (lOdeSolver . lSimParams) RK4
        f str | map toLower str == "adaptive"    = modifyShellSt $ set (lOdeSolver . lSimParams) Adaptive
        f _ = shellPutInfoLn "Possible options <euler, rk4, adaptive>"

    simSdeSolverCmd = cmd "sdeSolver" f "SDE Solver to use for simulation <em, reflectedem>"
      where
        f :: String -> Sh SysState ()
        f str | map toLower str == "em"          = modifyShellSt $ set (lSdeSolver . lSimParams) EM
        f str | map toLower str == "projem" = modifyShellSt $ set (lSdeSolver . lSimParams) ProjEM
        f _ = shellPutInfoLn "Possible options <em, reflectedem>"

    simBackendCmd = cmd "backend" f "Simulation backend to use <interpreter, jitcompiler, aotcompiler>"
      where
        f :: String -> Sh SysState ()
        f str | map toLower str == "interpreter"     = modifyShellSt $ set (lBackend . lSimParams) Interpreter
        f str | map toLower str == "jitcompiler"     = modifyShellSt $ set (lBackend . lSimParams) JITCompiler
        f str | map toLower str == "aotcompiler"     = modifyShellSt $ set (lBackend . lSimParams) AOTCompiler
        f str | map toLower str == "objectfile"      = modifyShellSt $ set (lBackend . lSimParams) ObjectFile
        f _ = shellPutInfoLn "Possible options <interpreter, jitcompiler, aotcompiler>"

    simExeFilenameCmd = cmd "exeOutput" f "Filename to save compiled model executable"
      where
        f :: File -> Sh SysState ()
        f (File x) = modifyShellSt $ set (lExeName . lSimParams) x

    simLinkerCmd = cmd "linker" f "System linker to use when compiling <dynamic, static>"
      where
        f :: String -> Sh SysState ()
        f str | map toLower str == "static"  = modifyShellSt $ set (lLinker . lSimParams) Static
        f str | map toLower str == "dynamic" = modifyShellSt $ set (lLinker . lSimParams) Dynamic
        f _ = shellPutInfoLn "Possible options <dynamic, static>"

    simExecuteCmd       = toggle "disableExecute" "Toggle Execution of Simulations" (get $ lExecute . lSimParams) (set $ lExecute . lSimParams)

    -- fast math
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

    simVecMathCmd       = toggle "vecMath" "Toggle Vectorisation Optimisation" (get $ lVecMath . lSimParams) (set $ lVecMath . lSimParams)

    -- opt params
    simOptimiseCmd      = toggle "disableOptimise" "Toggle LLVM Optimisation of Simulations" (get $ lOptimise . lSimParams) (set $ lOptimise . lSimParams)
    simShortCircuitCmd  = toggle "disableShortCircuit" "Toggle Short-circuiting of boolean operators (N.B. may change simulation semantics) " (get $ lOptShortCircuit . lSimParams) (set $ lOptShortCircuit . lSimParams)
    simPowerExpanCmd    = toggle "disablePowerExpan" "Toggle Expansion of pow() calls (requires mathModel = fast)" (get $ lOptPowerExpan . lSimParams) (set $ lOptPowerExpan . lSimParams)


-- Start Simulation ----------------------------------------------------------------------------------------------------
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
        Left err -> shellPutErrLn $ "Simulation Error - " ++ err
        -- update state and PRINT res
        Right (_, st') -> putShellSt st' >> shellPutInfoLn "Simulation complete"
    -- return, setting up new LOOP
    return ()

  where
    simulate' :: SysExceptIO ()
    simulate' = do
        -- first run a quick sanity check of the simulation params
        params <- getSysState lSimParams
        liftExSys $ checkParams params

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

    -- basic sanity checks over simulation parameters
    -- TODO - should this be placed elsewhere?
    checkParams :: SimParams -> MExcept ()
    checkParams SimParams{..} = do
        let simDuration = _stopTime - _startTime
        when (_outputPeriod < _timestep)
            (throwError $ printf "Output period (%g) must be equal or greater to timestep (%g)\n" _outputPeriod _timestep)
        when ((_backend == ObjectFile || _odeSolver == Adaptive) && _maxTimestep < _timestep)
            (throwError $ printf "Max timestep (%g) must be equal or greater to timestep (%g)\n" _maxTimestep _timestep)
        when (_stopTime <= _startTime)
            (throwError $ printf "Stop time (%g) must be greater than start time (%g)\n" _stopTime _startTime)
        when (_timestep > simDuration)
            (throwError $ printf "Timestep (%g) must be smaller or equal to simulation time interval (%g)\n" _timestep simDuration)
        when (_startTime < 0)
            (throwError $ printf "Start time (%g) cannot be negative \n" _startTime)
        when (_timestep < 0 || _maxTimestep < 0)
            (throwError $ printf "Timesteps (%g, %g) cannot be negative \n" _timestep _maxTimestep)
        when (simDuration / _timestep > 2 ** 52) -- 2** 52 = max int in a GHC double
            (throwError $ printf "Timestep (%g) is too small for the time duration (%g), not supported by fixed solvers\n" _timestep simDuration)

        return ()

