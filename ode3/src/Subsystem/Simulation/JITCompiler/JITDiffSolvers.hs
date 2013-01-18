-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Simulation.JITCompiler.JITDiffSolvers
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | A type-class that specifisy the requirements of a diffential eqn solver, and several implementations
--
-----------------------------------------------------------------------------

module Subsystem.Simulation.JITCompiler.JITDiffSolvers (
OdeSolver(..), Solver(..), EulerSolver, EulerMSolver, RK4Solver
) where

-- Labels
import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)

-- LLVM code
import LLVM.Wrapper.Core as LLVM hiding (constGEP)
import LLVM.Wrapper.BitWriter as LLVM
import qualified LLVM.FFI.Core as LFFI


import Data.Int
import Data.Word
import qualified Foreign as FFI
import qualified Foreign.C as FFI

import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Utils.OrdMap as OrdMap

import Control.Monad.State
import Utils.MonadSupply

import Utils.CommonImports
import qualified Subsystem.SysState as Sys

import AST.Common as AC
import AST.CoreFlat as CF

import Subsystem.Simulation.Common
import Subsystem.Simulation.JITCompiler.JITCommon
import Subsystem.Simulation.JITCompiler.JITModel

-- Solvers -------------------------------------------------------------------------------------------------------------

-- A solver class for abstracting over the differing code-gen requriements for Odes/Sdes
class OdeSolver a where
    genVals :: [Id] -> GenM a
    genSolver :: a -> LLVM.Value -> CF.Module -> GenM ()
    getStateVals :: a -> LocalMap

-- Existential wrapper for solver
data Solver :: * where
    MkSolver :: OdeSolver a => a -> Solver

instance OdeSolver Solver where
    genVals ids = genVals ids
    genSolver (MkSolver s) curTimeRef = genSolver s curTimeRef
    getStateVals (MkSolver s) = getStateVals s


-- Euler Solver --------------------------------------------------------------------------------------------------------

data EulerSolver = EulerSolver  { eulerStateVals :: LocalMap
                                , eulerDeltaVals :: LocalMap}


instance OdeSolver EulerSolver where
    genVals ids = do    -- create the vals
        stateRefMap <- createVals ids "StateRef"
        deltaRefMap <- createVals ids "DeltaRef"
        return $ EulerSolver stateRefMap deltaRefMap

    getStateVals e = eulerStateVals e

    genSolver (EulerSolver stateRefMap deltaRefMap) curTimeRef CF.Module{loopExprs, simOps} = do
        GenState {builder, curFunc, simParams} <- get
        -- gen the modelRHS code
        stateValMap <- loadRefMap stateRefMap
        _ <- withPtrVal builder curTimeRef $ \curTimeVal -> do
            genModelRHS loopExprs simOps curTimeVal stateValMap deltaRefMap Map.empty

        -- update the states/run the forward euler
        liftIO $ mapM_ (updateState builder simParams) simOps

      where
        updateState :: Builder -> Sys.SimParams -> SimOp -> IO ()
        updateState builder simParams (Ode i dV) = do
            -- get state val
            updatePtrVal builder (stateRefMap Map.! i) $ \stateVal -> do
                withPtrVal builder (deltaRefMap Map.! i) $ \dVal -> do
                    -- y' = y + h*dy
                    dValTime <- buildFMul builder dVal (constDouble $ L.get Sys.lTimestep simParams) "deltaTime"
                    buildFAdd builder stateVal dValTime "newState"



-- Euler Maruyama (SDE) Solver --------------------------------------------------------------------------------------------------------

data EulerMSolver = EulerMSolver    { eulerMStateVals :: LocalMap
                                    , eulerMDeltaVals :: LocalMap
                                    , eulerMWienerVals :: LocalMap
                                    }


instance OdeSolver EulerMSolver where
    genVals ids = do    -- create the vals
        stateRefMap  <- createVals ids "StateRef"
        deltaRefMap  <- createVals ids "DeltaRef"
        wienerRefMap <- createVals ids "WienerRef"
        return $ EulerMSolver stateRefMap deltaRefMap wienerRefMap

    getStateVals e = eulerMStateVals e

    genSolver (EulerMSolver stateRefMap deltaRefMap wienerRefMap) curTimeRef CF.Module{loopExprs, simOps} = do
        GenState {builder, curFunc, simParams, libOps} <- get
        -- gen the modelRHS code
        stateValMap <- loadRefMap stateRefMap
        _ <- withPtrVal builder curTimeRef $ \curTimeVal -> do
            genModelRHS loopExprs simOps curTimeVal stateValMap deltaRefMap wienerRefMap

        -- update the states/run the forward euler
        liftIO $ mapM_ (updateState builder simParams libOps) simOps

      where
        updateState :: Builder -> Sys.SimParams -> LibOps -> SimOp -> IO ()
        updateState builder simParams _ (Ode i _) = do
            -- get state val
            updatePtrVal builder (stateRefMap Map.! i) $ \stateVal -> do
                withPtrVal builder (deltaRefMap Map.! i) $ \dVal -> do
                    -- y' = y + h*dy
                    dValTime <- buildFMul builder dVal (constDouble $ L.get Sys.lTimestep simParams) "deltaTime"
                    buildFAdd builder stateVal dValTime "newState"

        updateState builder simParams libOps (Sde i _ _) = do
            -- get state val
            updatePtrVal builder (stateRefMap Map.! i) $ \stateVal -> do
                withPtrVal builder (wienerRefMap Map.! i) $ \wVal -> do
                    withPtrVal builder (deltaRefMap Map.! i) $ \dVal -> do
                        -- y' = y + h*dy + dW*sqrt(dt)*rand(0,1)
                        randVal <- buildCall builder (libOps Map.! "OdeRandNormal") [] ""
                        wiener1 <- buildFMul builder randVal (constDouble . sqrt $ L.get Sys.lTimestep simParams) "wiener1"
                        wiener2 <- buildFMul builder wiener1 wVal "wiener2"
                        delta1 <- buildFMul builder dVal (constDouble $ L.get Sys.lTimestep simParams) "delta1"
                        state1 <- buildFAdd builder wiener2 delta1 "state1"
                        buildFAdd builder stateVal state1 "state2"

-- RK4 Solver ----------------------------------------------------------------------------------------------------------

data RK4Solver = RK4Solver  { rk4StateVals :: LocalMap, rk4Delta1Vals :: LocalMap, rk4Delta2Vals :: LocalMap
                            , rk4Delta3Vals :: LocalMap, rk4Delta4Vals :: LocalMap}

instance OdeSolver RK4Solver where
    genVals ids = do    -- create the vals
        stateRefMap <- createVals ids "StateRef"
        deltaRef1Map <- createVals ids "DeltaRef1"
        deltaRef2Map <- createVals ids "DeltaRef2"
        deltaRef3Map <- createVals ids "DeltaRef3"
        deltaRef4Map <- createVals ids "DeltaRef4"
        return $ RK4Solver stateRefMap deltaRef1Map deltaRef2Map deltaRef3Map deltaRef4Map

    getStateVals e = rk4StateVals e

    -- TODO - we can reuse the same deltaVals for each step, as the final staate fot the step is held within kStates
    genSolver (RK4Solver stateRefMap deltaRef1Map deltaRef2Map deltaRef3Map deltaRef4Map) curTimeRef CF.Module{loopExprs, simOps} = do
        GenState {builder, curFunc, simParams} <- get

        -- deref the state vals and time - these are static during main RK4 body
        stateValMap <- loadRefMap stateRefMap
        let stateVals = Map.elems stateValMap

        -- time constants
        let h = constDouble $ L.get Sys.lTimestep simParams
        let hHalf = constFMul h $ constDouble 0.5
        t <- liftIO $ buildLoad builder curTimeRef "curTime"
        tPlusHHalf <- liftIO $ buildFAdd builder t hHalf "tPlusHHalf"
        tPlusH <- liftIO $ buildFAdd builder t h "tPlusH"

        -- GEN K1
        -- call the modelRHS func manually
        genModelRHS loopExprs simOps t stateValMap deltaRef1Map Map.empty
        k1State <- multByTimeStep builder deltaRef1Map h

        -- GEN K2
        k2State <- genKState builder stateValMap k1State deltaRef2Map tPlusHHalf h $ \(sv, k1v) -> do
            i <- buildFMul builder k1v (constDouble 0.5) ""
            buildFAdd builder sv i "sv"

        -- GEN K3 (same as K2)
        k3State <- genKState builder stateValMap k2State deltaRef3Map tPlusHHalf h $ \(sv, k2v) -> do
            i <- buildFMul builder k2v (constDouble 0.5) ""
            buildFAdd builder sv i "sv"

        -- GEN K4 (similar to K3)
        k4State <- genKState builder stateValMap k3State deltaRef4Map tPlusH h $ \(sv, k3v) -> do
            buildFAdd builder sv k3v "sv"

        -- update the states/calc avg. dy and adjust y for all SimOps
        forM_ simOps $ updateState builder k1State k2State k3State k4State
        return ()
      where
        -- | generate each of the k vals for RK4
        genKState :: Builder -> LocalMap -> LocalMap -> LocalMap -> Value -> Value -> ((Value, Value) -> IO Value) -> GenM LocalMap
        genKState builder stateValMap inKState deltaRefMap timeDelta h stateF = do
            -- generate the modified statevals map - y_n + k_x
            stateValMap' <- liftIO $ DT.mapM stateF (Map.intersectionWith (,) stateValMap inKState)
            -- call the loop func f(t,y')
            genModelRHS loopExprs simOps timeDelta stateValMap' deltaRefMap Map.empty
            -- k' = h * f(t,y')
            kState' <- multByTimeStep builder deltaRefMap h
            return kState'

        -- | multiply a vector of refs by the timestep, i.e. h*f(t,y)
        multByTimeStep :: Builder -> LocalMap -> Value -> GenM LocalMap
        multByTimeStep builder deltaRefMap timeStep = liftIO $ flip DT.mapM deltaRefMap $ \deltaRef ->
            withPtrVal builder deltaRef $ \deltaVal -> buildFMul builder deltaVal timeStep "deltaTime"

        -- | get & updatestate val using RK4 algo for y' = y + 1/6*(k1 + 2*k2 + 2*k3 + k4)
        updateState :: Builder -> LocalMap -> LocalMap -> LocalMap -> LocalMap -> SimOp -> GenM ()
        updateState builder k1State k2State k3State k4State (Ode i dV) = liftIO $
            updatePtrVal builder (stateRefMap Map.! i) $ \stateVal -> do
                -- let idx = idMap Map.! i
                -- muls -- TODO - replace muls with adds?
                kTmpMul <- buildFMul builder (k2State Map.! i) (constDouble 2.0) "kTmpMul"
                kTmpMul1 <- buildFMul builder (k3State Map.! i) (constDouble 2.0) "kTmpMul1"
                -- adds
                kTmpAdd <- buildFAdd builder (k1State Map.! i) kTmpMul "kTmpAdd"
                kTmpAdd1 <- buildFAdd builder kTmpAdd kTmpMul1 "kTmpAdd1"
                kTmpAdd2 <- buildFAdd builder (k4State Map.! i) kTmpAdd1 "kTmpAdd2"
                -- update state val
                kTmpTotal  <- buildFDiv builder kTmpAdd2 (constDouble $ 6.0) "kTmpTotal"
                buildFAdd builder stateVal kTmpTotal "newState"
