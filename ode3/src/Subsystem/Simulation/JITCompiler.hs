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
import LLVM.Wrapper.Core
import LLVM.Wrapper.BitWriter

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

import qualified AST.Common as AC
import qualified AST.CoreFlat as CF

import Subsystem.Simulation.Common


type SimM = StateT SimState MExceptIO

data SimState = SimState { unused :: Bool }
                deriving (Show, Eq)


compile :: CF.Module -> Sys.SysExceptIO ()
compile mod = do

    -- setup the default simulation state
    p <- Sys.getSysState Sys.lSimParams
    liftIO $ debugM "ode3.sim" $ "JIT Compiling Model"

    -- compile the module
    liftIO $ createJITModule mod

    -- configure LLVM
    -- liftIO $ initializeNativeTarget

    liftIO $ debugM "ode3.sim" $ "Starting JIT Simulation"
    -- run the simulation
    liftIO $ runSimulation


    -- close any output files? (handle within LLVM code)
    liftIO $ debugM "ode3.sim" $ "JIT Simulation Complete"
    return ()
  where



-- JIT Setup -----------------------------------------------------------------------------------------------------------


runSimulation :: IO ()
runSimulation = return ()

createJITModule :: CF.Module -> IO ()
createJITModule mod = do
    -- gen the JIT funcs

    -- save the module to disk
    mMod <- moduleCreateWithName "model"
    -- insert the global vals
    _ <- addGlobal mMod (arrayType doubleType 3) "statesVec"
    _ <- addGlobal mMod (arrayType doubleType 3) "deltasVec"

    -- insert the funcs into the module
    _ <- createTestFunc mMod

    -- defineModule mMod $ genModelInitials mod
    -- defineModule mMod $ genModelDeltas mod
    printModuleToFile mMod "model.ll"
    writeBitcodeToFile mMod "model.bc"
    -- run our external script - this runs our optimisations and links to the Ode run-time library

    -- return the update module
    return ()


-- Code Generation -----------------------------------------------------------------------------------------------------
-- We only codegen the initial val and delta fucntion calculation, other funcs provided within the std. library for now
-- this includes init/startup and shutdown funcs, and the solvers (for now a forward Euler and RK4)

--genStateVector :: [Double] -> TGlobal (Array n Double)
--genStateVector x = createNamedGlobal False ExternalLinkage "StatesVec" (constOf (Array x))

--
---- simulation function - calcs the initial values for the state vals (y = g(t))
--genModelInitials :: CF.Module -> CodeGenModule (Function (Double -> Ptr Double -> IO ()))
--genModelInitials mod = createNamedFunction ExternalLinkage "modelInitials" $ \ time outStates ->
--    do  t <- add time time
--        ret ()
--
---- simulation function - calcs the delta funcs for the state vals (dy = f(t, y))
--genModelDeltas :: CF.Module -> CodeGenModule (Function (Double -> Ptr Double -> Ptr Double -> IO ()))
--genModelDeltas mod = createNamedFunction ExternalLinkage "modelDeltas" $ \ time inStates outDeltas ->
--    do  t <- add time time
--        ret ()


createTestFunc :: Module -> IO Value
createTestFunc mod = do
    -- define the func
    f <- addFunction mod "test" (functionType int64Type [] False)
    builder <- createBuilder
    -- create the entry block & pos the builder
    entryBB <- appendBasicBlock f "entry"
    positionAtEnd builder entryBB

    -- add the insts
    v <- buildAdd builder (constInt int64Type 1 False) (constInt int64Type 2 False) "tmp"
    buildRet builder v






