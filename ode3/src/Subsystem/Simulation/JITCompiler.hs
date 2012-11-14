-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Simulation.JITCompiler
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Takes a CoreFlat AST and simulates is using an JIT Compiler with a Forward Euler
-- This module includes the hihg-level functions generated by the JIT compiler and related
-- machinary for setting up and running a simulation
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Subsystem.Simulation.JITCompiler (
compileAndSimulate
) where

-- Labels
import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)

-- LLVM code
import LLVM.Wrapper.Core as LLVM
import LLVM.Wrapper.BitWriter as LLVM
import LLVM.Wrapper.ExecutionEngine as LLVM
import qualified LLVM.FFI.Core as LFFI
import qualified LLVM.Target.Native as LFFI

import Data.Int
import Data.Word
import qualified Foreign as FFI
import qualified Foreign.C as FFI

import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import qualified Data.Map as Map
import qualified Utils.OrdMap as OrdMap

import qualified Shelly as Sh

import Control.Monad.State
import Utils.MonadSupply

import Utils.CommonImports
import qualified Subsystem.SysState as Sys

import AST.Common as AC
import AST.CoreFlat as CF

import Subsystem.Simulation.JITCompiler.JITCommon
import Subsystem.Simulation.JITCompiler.JITSolver
import Subsystem.Simulation.JITCompiler.JITShell


-- Entry ---------------------------------------------------------------------------------------------------------------

compileAndSimulate :: CF.Module -> Sys.SysExceptIO ()
compileAndSimulate mod = do
    p <- Sys.getSysState Sys.lSimParams

    -- Compiler stage
    liftIO $ debugM "ode3.sim" $ "Compiling and linking Model and Sim code"
    -- all code that runs in the GenM monad
    lift $ runStateT (runGenM $ genLLVMModule p mod >> linkLLVMModule p) $ mkGenState p

    -- Simulate stage - assumes presence of a sim.bc file
    liftIO $ debugM "ode3.sim" $ "Starting (Compiled) Simulation"
    -- determine the correct compile/simulate options
    case (L.get Sys.lBackend p) of
        -- only dynamic-linking, w/execution allowed in JITCompiler
        Sys.JITCompiler -> liftIO $ runJITSimulation p
        Sys.AOTCompiler -> liftIO $ runAOTScript p

    -- close any output files? (handle within LLVM code)
    liftIO $ debugM "ode3.sim" $ "(Compiled) Simulation Complete"

-- JIT Interface -------------------------------------------------------------------------------------------------------

-- | Compile a CoreFlat Ode module into a LLVM model
genLLVMModule :: Sys.SimParams -> CF.Module -> GenM ()
genLLVMModule p odeMod = do
    -- create the module
    llvmMod <- liftIO $ moduleCreateWithName "model"
    -- insert the math ops and lib ops
    (mathOps, libOps) <- liftIO $ defineExtOps p llvmMod
    modify (\st -> st { llvmMod, mathOps, libOps })

    -- generate & insert the funcs into the module
    initsF <-   genModelInitials odeMod
    loopF <-    genModelLoop odeMod
    simF <-     genModelSolver odeMod initsF loopF

    -- gen a main func if standalone AOT compiling
    when (L.get Sys.lBackend p == Sys.AOTCompiler) $ genAOTMain simF

    -- save the module to disk
    liftIO $ printModuleToFile llvmMod "Model.ll"
    liftIO $ writeBitcodeToFile llvmMod "Model.bc"
    return ()

-- | Calls out to our linker script
linkLLVMModule :: Sys.SimParams -> GenM ()
linkLLVMModule p = Sh.shelly . Sh.verbosely $ llvmLinkScript p

-- | Load our compiled module and run a simulation
runJITSimulation :: Sys.SimParams -> IO ()
runJITSimulation p = do
    liftIO $ warningM "ode3.sim" $ "JIT Simulation is experimental - only GNU linking tested"

    -- load the linked/optimised module
    simMod <- readBitcodeFromFile "./Sim.bc"

    -- setup the jitter
    LFFI.initializeNativeTarget
    ee <- createJITCompilerForModule simMod (fromIntegral 3)

    -- get and call the entry func
    modelSolverFunc <- fromJust <$> findFunction ee "modelSolver"
    _ <- runFunction ee modelSolverFunc (fromIntegral 0) []

    -- destroy Module
    disposeModule simMod

    -- DEBUG - also write the dyn exe to disk
    --liftIO $ debugM "ode3.sim" $ "Writing executable to disk"
    --runAOTScript $ p { Sys._linker = Sys.DynamicLink, Sys._execute = False }
    return ()

-- | Calls out to our AOT script
runAOTScript :: Sys.SimParams -> IO ()
runAOTScript p = Sh.shelly . Sh.verbosely $ llvmAOTScript p

