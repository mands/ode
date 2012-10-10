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
--
-----------------------------------------------------------------------------

module Subsystem.Simulation.JITCompiler (
compile
) where

-- Labels
import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)

-- LLVM code
-- import LLVM.Core
-- import LLVM.ExecutionEngine
-- import LLVM.Util.Optimize
import Data.Int
import Data.Word


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

import Subsystem.Simulation.Common


type SimM = StateT SimState MExceptIO

data SimState = SimState { unused :: Bool }
                deriving (Show, Eq)


compile :: CF.Module -> Sys.SysExceptIO ()
compile mod = do
    undefined
{--
    -- setup the default simulation state
    p <- Sys.getSysState Sys.lSimParams
    liftIO $ debugM "ode3.sim" $ "Compiling Simulation"

    -- configure LLVM
    liftIO $ initializeNativeTarget
    -- compile the module

    -- run the simulation

    -- close any output files? (handle within LLVM code)
    liftIO $ debugM "ode3.sim" $ "Simulation Complete"
    return ()
  where



---- genrates the initial function that setups the state vals
--genInitFunc :: CF.Module -> CodeGenModule (Function (Int64 -> Int64 -> IO Int64))
--genInitFunc mod = createFunction ExternalLinkage $ \ x y ->
--    do  t <- add x y
--        ret t
--
--genShutdownFunc :: CF.Module -> CodeGenModule (Function (Int64 -> Int64 -> IO Int64))
--genShutdownFunc mod = createFunction ExternalLinkage $ \ x y ->
--    do  t <- add x y
--        ret t

-- simulation function - runs the ODEs/updates state
-- can we put this as precompiled bitcode?
genSimulateFunc :: CF.Module -> CodeGenModule (Function (Int64 -> Int64 -> IO Int64))
genSimulateFunc mod = createFunction ExternalLinkage $ \ x y ->
    do  t <- add x y
        ret t

-- simulation function - runs the ODEs
-- can we put this as precompiled bitcode?
genModelInit :: CF.Module -> CodeGenModule (Function (Int64 -> Int64 -> IO Int64))
genModelInit mod = createFunction ExternalLinkage $ \ x y ->
    do  t <- add x y
        ret t

-- simulation function - runs the ODEs
-- can we put this as precompiled bitcode?
genModelLoop :: CF.Module -> CodeGenModule (Function (Int64 -> Int64 -> IO Int64))
genModelLoop mod = createFunction ExternalLinkage $ \ x y ->
    do  t <- add x y
        ret t
--}
