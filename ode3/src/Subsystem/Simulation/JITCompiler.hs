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
compile
) where

-- Labels
import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)

-- LLVM code
import LLVM.Wrapper.Core as LLVM
import LLVM.Wrapper.BitWriter as LLVM
import qualified LLVM.FFI.Core as LFFI


import Data.Int
import Data.Word
import qualified Foreign as FFI
import qualified Foreign.C as FFI

import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import qualified Data.Map as Map
import qualified Utils.OrdMap as OrdMap

import Control.Monad.State
import Utils.MonadSupply

import Utils.CommonImports
import qualified Subsystem.SysState as Sys

import AST.Common as AC
import AST.CoreFlat as CF

import Subsystem.Simulation.JITCompiler.JITCommon
import Subsystem.Simulation.JITCompiler.JITMain


-- Entry ---------------------------------------------------------------------------------------------------------------

compile :: CF.Module -> Sys.SysExceptIO ()
compile mod = do

    -- setup the default simulation state
    p <- Sys.getSysState Sys.lSimParams
    liftIO $ debugM "ode3.sim" $ "JIT Compiling Model"


    -- configure LLVM
    -- liftIO $ initializeNativeTarget
    lift $ runStateT (runGenM codeGen) $ mkGenState p


    liftIO $ debugM "ode3.sim" $ "Starting JIT Simulation"
    -- run the simulation
    liftIO $ runSimulation

    -- close any output files? (handle within LLVM code)
    liftIO $ debugM "ode3.sim" $ "JIT Simulation Complete"
    return ()
  where
    -- all code that runs in the SimM monad
    codeGen :: GenM ()
    codeGen = do
        createJITModule mod


