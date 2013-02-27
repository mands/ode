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
OdeSolver(..), Solver(..), EulerSolver, EulerMSolver, ProjSolver, RK4Solver
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
import Data.Maybe(mapMaybe)
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
    genVals :: [Id] -> [SimOp] -> GenM a
    genSolver :: a -> LLVM.Value -> CF.Module -> GenM ()
    getStateVals :: a -> LocalMap

-- Existential wrapper for solver
data Solver :: * where
    MkSolver :: OdeSolver a => a -> Solver

instance OdeSolver Solver where
    genVals ids simOps = genVals ids simOps
    genSolver (MkSolver s) curTimeRef = genSolver s curTimeRef
    getStateVals (MkSolver s) = getStateVals s


-- Euler Solver --------------------------------------------------------------------------------------------------------

data EulerSolver = EulerSolver  { eulerStateVals :: LocalMap
                                , eulerDeltaVals :: LocalMap}


instance OdeSolver EulerSolver where
    genVals ids _ = do    -- create the vals
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

        updateState builder simParams _ = return ()


-- Euler Maruyama (SDE) Solver --------------------------------------------------------------------------------------------------------

data EulerMSolver = EulerMSolver    { eulerMStateVals :: LocalMap
                                    , eulerMDeltaVals :: LocalMap
                                    , eulerMWienerVals :: LocalMap
                                    }


instance OdeSolver EulerMSolver where
    genVals ids _ = do    -- create the vals
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
                        -- y' = y + h*dy + dW (where dW should include a wiener val)
                        -- prev implicit weiner process generation
                        -- randVal <- buildCall builder (libOps Map.! "OdeRandNormal") [] ""
                        -- wiener1 <- buildFMul builder randVal (constDouble . sqrt $ L.get Sys.lTimestep simParams) "wiener1"
                        -- wiener2 <- buildFMul builder wiener1 wVal "wiener2"
                        delta1 <- buildFMul builder dVal (constDouble $ L.get Sys.lTimestep simParams) "delta1"
                        state1 <- buildFAdd builder wVal delta1 "state1"
                        buildFAdd builder stateVal state1 "state2"

-- Euler-Maruyama Projected (SDE) Solver --------------------------------------------------------------------------------------------------------

data ProjSolver = ProjSolver    { projStateVals :: LocalMap
                                , projDeltaVals :: LocalMap
                                , projWienerVals :: LocalMap
                                , projArray :: LLVM.Value
                                }

instance OdeSolver ProjSolver where
    genVals ids simOps = do    -- create the vals
        stateRefMap  <- createVals ids "StateRef"
        deltaRefMap  <- createVals ids "DeltaRef"
        wienerRefMap <- createVals ids "WienerRef"

        -- build an array to hold the values
        GenState {builder} <- get
        -- projArr <- FFI.withCString "projArray" $ \cStr ->
            -- LFFI.buildArrayAlloca builder (arrayType doubleType (fromIntegral $ length projIds)) projArrSize cStr
            -- LFFI.buildArrayAlloca builder doubleType projArrSize cStr
        projArr <- liftIO $ buildAlloca builder (arrayType doubleType $ fromIntegral projIdsSize) "projArray"
        return $ ProjSolver stateRefMap deltaRefMap wienerRefMap projArr
      where
        projIdsSize = length . filter (\op -> case op of (Sde i _ _) -> True; _ -> False) $ simOps

    getStateVals e = projStateVals e

    genSolver (ProjSolver stateRefMap deltaRefMap wienerRefMap projArr) curTimeRef CF.Module{loopExprs, simOps} = do
        GenState {builder, curFunc, simParams, libOps} <- get
        -- gen the modelRHS code
        stateValMap <- loadRefMap stateRefMap
        _ <- withPtrVal builder curTimeRef $ \curTimeVal -> do
            genModelRHS loopExprs simOps curTimeVal stateValMap deltaRefMap wienerRefMap

        -- update the states/run the forward euler
        liftIO $ mapM_ (updateState builder simParams libOps) simOps

        -- run the max/min state val check
        projBB <- liftIO $ appendBasicBlock curFunc "projBB"
        endBB <- liftIO $ appendBasicBlock curFunc "endBB"
        stateMinMaxCheck builder curFunc projBB endBB
        -- _ <- liftIO $ buildBr builder projBB
        -- now run the projection function for all init vals bound to SDEs (not just groups)
        genProjVal builder libOps projBB endBB
        -- make sure we're in the endBB
        positionAtEnd' builder endBB
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
                        -- y' = y + h*dy + dW (where dW should include a wiener val)
                        delta1 <- buildFMul builder dVal (constDouble $ L.get Sys.lTimestep simParams) "delta1"
                        state1 <- buildFAdd builder wVal delta1 "state1"
                        buildFAdd builder stateVal state1 "state2"

        -- run the project function
        genProjVal :: Builder -> LibOps -> BasicBlock -> BasicBlock -> GenM ()
        genProjVal builder libOps projBB endBB = do
            positionAtEnd' builder projBB
            -- store the values in the array
            gatherArray builder projArr projIds
            -- call the project function
            arrRef <- liftIO $ buildInBoundsGEP builder projArr [constInt64 0, constInt64 0] "arrRef"
            liftIO . void $ buildCall builder (libOps Map.! "OdeProjectVector") [arrRef, projArrSize] ""
            -- unload the values from the array
            scatterArray builder projArr projIds
            liftIO . void $ buildBr builder endBB

        -- find the valueRef of all stateVals pointed to by an SDE (NOTE - this is why can only have one SDE subsystem in solver)
        projIds = mapMaybe (\op -> case op of (Sde i _ _) -> Just (stateRefMap Map.! i); _ -> Nothing) simOps
        projArrSize = constInt64 $ length projIds

        -- min and max checks (and optional sum == 1 check)
        -- TODO - abstract forloop/multi-BB branch logic into LLVM combinator
        stateMinMaxCheck :: Builder -> LLVM.Value -> BasicBlock -> BasicBlock -> GenM ()
        stateMinMaxCheck builder curFunc projBB endBB = do
            startBB <- liftIO $ appendBasicBlock curFunc "startBB"
            liftIO $ buildBr builder startBB
            positionAtEnd' builder startBB
            -- run the loop
            forM projIds $ \projIdRef -> do
                withPtrVal builder projIdRef $ \projId -> do
                    gtZero <- liftIO $ buildFCmp builder FPOGE projId constZero "greaterZero"
                    lsOne <- liftIO $ buildFCmp builder FPOLE projId constOne "lessOne"
                    inBounds <- liftIO $ buildAnd builder gtZero lsOne "inBounds"

                    nextBB <- liftIO $ appendBasicBlock curFunc "nextBB"
                    liftIO $ buildCondBr builder inBounds nextBB projBB
                    positionAtEnd' builder nextBB
            -- sum loop, debug only
            runSumCheck

            -- branch to end
            liftIO $ buildBr builder endBB

            return ()
          where
            -- this invovles a second loop - should integrate with the max/min loop, but f'it
            runSumCheck :: GenM ()
            runSumCheck = do
                GenState {libOps} <- get
                -- collect the sum vals
                (Just sumProjs) <- DF.foldlM sumCheck Nothing projIds
                -- instead of costly fabs, gen 2 cmps => 1-eps < sumProjs < 1+eps
                let epsilon = constDouble 1e-9
                cond1 <- liftIO $ buildFCmp builder FPOLE sumProjs (constFAdd constOne epsilon) "upperBound"
                cond2 <- liftIO $ buildFCmp builder FPOGE sumProjs (constFSub constOne epsilon) "lowerBound"
                inBounds <- liftIO $ buildAnd builder cond1 cond2 "inBounds"

                _ <- ifStmt builder curFunc inBounds
                    (buildNoOp)
                    -- doesn't sum to 1 (within epsilon), exit
                    (\builder -> do
                        liftIO $ debugStmt builder libOps "ERROR - Vals do not sum to 1 - %g\n" [sumProjs]
                        buildNoOp builder
                        liftIO $ buildCall builder (libOps Map.! "exit") [constInt64 1] "")
                return ()

            sumCheck :: (Maybe LLVM.Value) -> LLVM.Value -> GenM (Maybe LLVM.Value)
            sumCheck Nothing projIdRef = withPtrVal builder projIdRef $ \projId -> return (Just projId)
            sumCheck (Just curAccum) projIdRef = withPtrVal builder projIdRef $ \projId -> do
                a <- liftIO $ buildFAdd builder curAccum projId "curAccum"
                return $ Just a



-- RK4 Solver ----------------------------------------------------------------------------------------------------------

data RK4Solver = RK4Solver  { rk4StateVals :: LocalMap, rk4Delta1Vals :: LocalMap, rk4Delta2Vals :: LocalMap
                            , rk4Delta3Vals :: LocalMap, rk4Delta4Vals :: LocalMap}

instance OdeSolver RK4Solver where
    genVals ids _ = do    -- create the vals
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
