-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Simulation.JITCompiler.JITMain
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Subsystem.Simulation.JITCompiler.JITSolver (
genModelSolver, genSSASolver, genAOTMain, genFFIParams, genFFIModelInitials, genFFIModelRHS
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


-- Code Generation -----------------------------------------------------------------------------------------------------
-- This code generates the custom solvers implemented in Ode, inlcuding a FEuler and RK4, plus extra code for generating
-- a standalone object file and the FFI

-- | A stub main function used for AOT compilation
genAOTMain :: LLVM.Value -> GenM ()
genAOTMain simF = do
    (curFunc, builder) <- genFunction "main" int32Type []
    -- call the sim func
    _ <- liftIO $ buildCall builder simF [] ""
    liftIO $ buildRet builder $ constInt32 0
    return ()


-- | C-compatible wrapper for all important simulation parameters - look at OdeModel.h for interface details
-- Write sim param constant to global vals within the module
genFFIParams :: Int -> CF.SimType -> GenM ()
genFFIParams numParams simType = do
    GenState {builder, simParams, llvmMod} <- get
    let Sys.SimParams{..} = simParams
    liftIO $ addGlobalWithInit llvmMod (constDouble $ _startTime) doubleType True "OdeParamStartTime"
    liftIO $ addGlobalWithInit llvmMod (constDouble $ _stopTime) doubleType True "OdeParamStopTime"
    liftIO $ addGlobalWithInit llvmMod (constDouble $ _timestep) doubleType True "OdeParamTimestep"
    liftIO $ addGlobalWithInit llvmMod (constDouble $ Sys.calcAdjustedStopTime simParams) doubleType True "OdeParamAdjustedStopTime"

    -- adaptive params
    liftIO $ addGlobalWithInit llvmMod (constDouble $ _maxTimestep) doubleType True "OdeParamMaxTimestep"
    liftIO $ addGlobalWithInit llvmMod (constInt64 $ _maxNumSteps) int64Type True "OdeParamMaxNumSteps"
    liftIO $ addGlobalWithInit llvmMod (constDouble $ _relError) doubleType True "OdeParamRelativeError"
    liftIO $ addGlobalWithInit llvmMod (constDouble $ _absError) doubleType True "OdeParamAbsoluteError"
    case _modelType of
        Sys.Stiff       -> liftIO $ addGlobalWithInit llvmMod (constInt32 $ 0) int32Type True "OdeParamModelType"
        Sys.NonStiff    -> liftIO $ addGlobalWithInit llvmMod (constInt32 $ 1) int32Type True "OdeParamModelType"

    -- output params
    liftIO $ addGlobalWithInit llvmMod (constDouble $ _outputPeriod) doubleType True "OdeParamPeriod"
    liftIO $ addGlobalWithInit llvmMod (constInt64 $ Sys.calcOutputInterval simParams) int64Type True "OdeParamPeriodInterval"

    -- HACK to build global string outside of a func (as buildGlob alString fails)
    liftIO $ addGlobalWithInit llvmMod (constString _filename False) (arrayType int8Type (fromIntegral $ length _filename + 1)) True "OdeParamOutput"
    -- liftIO $ buildGlobalString builder "test" "test" -- (Sys._filename  simParams) "OdeParamOutput"

    -- sim type
    case simType of
        CF.SimODE   -> liftIO $ addGlobalWithInit llvmMod (constInt32 $ 0) int32Type True "OdeParamSimType"
        CF.SimSDE   -> liftIO $ addGlobalWithInit llvmMod (constInt32 $ 1) int32Type True "OdeParamSimType"
        CF.SimRRE   -> liftIO $ addGlobalWithInit llvmMod (constInt32 $ 2) int32Type True "OdeParamSimType"

    -- model size
    liftIO $ addGlobalWithInit llvmMod (constInt64 $ numParams) int64Type True "OdeParamStateSize"
    return ()


-- | C-compatible wrapper for the initial values function
genFFIModelInitials :: LLVM.Value -> Int -> GenM ()
genFFIModelInitials initsF numParams = do
    (curFunc, builder) <- genFunction "OdeModelInitials" voidType createArgsList
    liftIO $ setFuncParam curFunc 1 [NoAliasAttribute, NoCaptureAttribute]

    (curTimeVal : stateArrayRef : []) <- liftIO $ LLVM.getParams curFunc

    -- use GEP to build list of ptrs into stateVals array, can pass direct into modelInits
    args <- liftIO $ forM [0..(numParams-1)] $ \idx ->
        buildInBoundsGEP builder stateArrayRef [constInt64 0, constInt64 idx] "stateValRef"

    -- call the internal inits functions
    liftIO $ buildCall builder initsF (curTimeVal : args ) ""
    liftIO $ buildRetVoid builder
    return ()
  where
    -- create the input args
    createArgsList = [doubleType, pointerType (arrayType doubleType (fromIntegral numParams)) 0]


-- | C-compatible wrapper for the RHS function
genFFIModelRHS :: LLVM.Value -> Int -> CF.SimType -> GenM ()
genFFIModelRHS rhsF numParams simType = do
    (curFunc, builder) <- genFunction "OdeModelRHS" voidType createArgsList
    liftIO $ setFuncParam curFunc 1 [NoAliasAttribute, NoCaptureAttribute]
    liftIO $ setFuncParam curFunc 2 [NoAliasAttribute, NoCaptureAttribute]
    liftIO $ setFuncParam curFunc 3 [NoAliasAttribute, NoCaptureAttribute]

    (curTimeVal : stateArrayRef : deltaArrayRef : weinerArrayRef : []) <- liftIO $ LLVM.getParams curFunc

    -- use GEP to build list of ptrs into stateVals array, need to deref each one
    stateArgs <- liftIO $ forM [0..arraySize] $ \idx -> do
        stateRef <- buildInBoundsGEP builder stateArrayRef [constInt64 0, constInt64 idx] "stateValRef"
        buildLoad builder stateRef "stateVal"

    -- use GEP to build list of ptrs into deltaVals array, can pass direct into modelLoop
    deltaArgs <- liftIO $ forM [0..arraySize] $ \idx ->
        buildInBoundsGEP builder deltaArrayRef [constInt64 0, constInt64 idx] "deltaValRef"

    -- use GEP to build list of ptrs into weinerVals array, can pass direct into modelLoop
    weinerArgs <- if simType == CF.SimSDE
                    then liftIO $ forM [0..arraySize] $ \idx ->
                        buildInBoundsGEP builder weinerArrayRef [constInt64 0, constInt64 idx] "weinerValRef"
                    else return []

    -- call the internal inits functions
    liftIO $ buildCall builder rhsF (curTimeVal : stateArgs ++ deltaArgs ++ weinerArgs) ""
    liftIO $ buildRetVoid builder
    return ()
  where
    arraySize = numParams - 1
    -- create the input args
    createArgsList = [doubleType, pointerType (arrayType doubleType (fromIntegral numParams)) 0
                                , pointerType (arrayType doubleType (fromIntegral numParams)) 0
                                , pointerType (arrayType doubleType (fromIntegral numParams)) 0]


-- Solvers -------------------------------------------------------------------------------------------------------------

-- A solver class for abstracting over the differing code-gen requriements for Odes/Sdes
class OdeSolver a where
    genVals :: [Id] -> GenM a
    genSolver :: a -> LLVM.Value -> LLVM.Value -> [SimOps] -> GenM ()
    getStateVals :: a -> ParamMap

-- Existential wrapper for solver
data Solver :: * where
    MkSolver :: OdeSolver a => a -> Solver

instance OdeSolver Solver where
    genVals ids = genVals ids
    genSolver (MkSolver s) rhsF curTimeRef = genSolver s rhsF curTimeRef
    getStateVals (MkSolver s) = getStateVals s


-- Euler Solver --------------------------------------------------------------------------------------------------------

data EulerSolver = EulerSolver  { eulerStateVals :: ParamMap
                                , eulerDeltaVals :: ParamMap}


instance OdeSolver EulerSolver where
    genVals ids = do    -- create the vals
        stateValRefMap <- createVals ids "StateRef"
        deltaValRefMap <- createVals ids "DeltaRef"
        return $ EulerSolver stateValRefMap deltaValRefMap

    getStateVals e = eulerStateVals e

    genSolver (EulerSolver stateValRefMap deltaValRefMap) rhsF curTimeRef simOps = do
        GenState {builder, curFunc, simParams} <- get
        -- call the modelRHS func
        stateVals <- mapM (\v -> liftIO $ buildLoad builder v "odeValx") $ OrdMap.elems stateValRefMap
        _ <- liftIO $ withPtrVal builder curTimeRef $ \curTime -> do
            buildCall builder rhsF (curTime : stateVals  ++ OrdMap.elems deltaValRefMap) ""

        -- update the states/run the forward euler
        liftIO $ mapM_ (updateState builder simParams) simOps

      where
        updateState :: Builder -> Sys.SimParams -> SimOps -> IO ()
        updateState builder simParams (Ode i dV) = do
            -- get state val
            updatePtrVal builder (stateValRefMap OrdMap.! i) $ \stateVal -> do
                withPtrVal builder (deltaValRefMap OrdMap.! i) $ \dVal -> do
                    -- y' = y + h*dy
                    dValTime <- buildFMul builder dVal (constDouble $ L.get Sys.lTimestep simParams) "deltaTime"
                    buildFAdd builder stateVal dValTime "newState"



-- Euler Maruyama (SDE) Solver --------------------------------------------------------------------------------------------------------

data EulerMSolver = EulerMSolver    { eulerMStateVals :: ParamMap
                                    , eulerMDeltaVals :: ParamMap
                                    , eulerMWeinerVals :: ParamMap
                                    }


instance OdeSolver EulerMSolver where
    genVals ids = do    -- create the vals
        stateValRefMap  <- createVals ids "StateRef"
        deltaValRefMap  <- createVals ids "DeltaRef"
        weinerValRefMap <- createVals ids "WeinerRef"
        return $ EulerMSolver stateValRefMap deltaValRefMap weinerValRefMap

    getStateVals e = eulerMStateVals e

    genSolver (EulerMSolver stateValRefMap deltaValRefMap weinerValRefMap) rhsF curTimeRef simOps = do
        GenState {builder, curFunc, simParams, libOps} <- get
        -- call the modelRHS func
        stateVals <- mapM (\v -> liftIO $ buildLoad builder v "odeValx") $ OrdMap.elems stateValRefMap
        _ <- liftIO $ withPtrVal builder curTimeRef $ \curTime -> do
            buildCall builder rhsF (curTime : stateVals  ++ OrdMap.elems deltaValRefMap ++ OrdMap.elems weinerValRefMap) ""

        -- update the states/run the forward euler
        liftIO $ mapM_ (updateState builder simParams libOps) simOps

      where
        updateState :: Builder -> Sys.SimParams -> LibOps -> SimOps -> IO ()
        updateState builder simParams _ (Ode i _) = do
            -- get state val
            updatePtrVal builder (stateValRefMap OrdMap.! i) $ \stateVal -> do
                withPtrVal builder (deltaValRefMap OrdMap.! i) $ \dVal -> do
                    -- y' = y + h*dy
                    dValTime <- buildFMul builder dVal (constDouble $ L.get Sys.lTimestep simParams) "deltaTime"
                    buildFAdd builder stateVal dValTime "newState"

        updateState builder simParams libOps (Sde i _ _) = do
            -- get state val
            updatePtrVal builder (stateValRefMap OrdMap.! i) $ \stateVal -> do
                withPtrVal builder (weinerValRefMap OrdMap.! i) $ \wVal -> do
                    withPtrVal builder (deltaValRefMap OrdMap.! i) $ \dVal -> do
                        -- y' = y + h*dy + dW*sqrt(dt)*rand(0,1)
                        randVal <- buildCall builder (libOps Map.! "OdeRandNormal") [] ""
                        weiner1 <- buildFMul builder randVal (constDouble . sqrt $ L.get Sys.lTimestep simParams) "weiner1"
                        weiner2 <- buildFMul builder weiner1 wVal "weiner2"
                        delta1 <- buildFMul builder dVal (constDouble $ L.get Sys.lTimestep simParams) "delta1"
                        state1 <- buildFAdd builder weiner2 delta1 "state1"
                        buildFAdd builder stateVal state1 "state2"

-- RK4 Solver ----------------------------------------------------------------------------------------------------------

data RK4Solver = RK4Solver  { rk4StateVals :: ParamMap, rk4Delta1Vals :: ParamMap, rk4Delta2Vals :: ParamMap
                            , rk4Delta3Vals :: ParamMap, rk4Delta4Vals :: ParamMap}

instance OdeSolver RK4Solver where
    genVals ids = do    -- create the vals
        stateValRefMap <- createVals ids "StateRef"
        deltaVal1RefMap <- createVals ids "DeltaRef1"
        deltaVal2RefMap <- createVals ids "DeltaRef2"
        deltaVal3RefMap <- createVals ids "DeltaRef3"
        deltaVal4RefMap <- createVals ids "DeltaRef4"
        return $ RK4Solver stateValRefMap deltaVal1RefMap deltaVal2RefMap deltaVal3RefMap deltaVal4RefMap

    getStateVals e = rk4StateVals e

    -- TODO - we can reuse the same deltaVals for each step, as the final staate fot the step is held within kStates
    genSolver (RK4Solver stateValRefMap deltaVal1RefMap deltaVal2RefMap deltaVal3RefMap deltaVal4RefMap) rhsF curTimeRef simOps = do
        GenState {builder, curFunc, simParams} <- get

        -- deref the state vals and time - these are static during main RK4 body
        stateVals <- mapM (\ref -> liftIO $ buildLoad builder ref "stateVal") $ OrdMap.elems stateValRefMap

        -- time constants
        let h = constDouble $ L.get Sys.lTimestep simParams
        let hHalf = constFMul h $ constDouble 0.5
        t <- liftIO $ buildLoad builder curTimeRef "curTime"
        tPlusHHalf <- liftIO $ buildFAdd builder t hHalf "tPlusHHalf"
        tPlusH <- liftIO $ buildFAdd builder t h "tPlusH"

        -- GEN K1
        -- call the modelRHS func manually
        callRHSF builder t stateVals deltaVal1RefMap
        k1State <- multByTimeStep builder deltaVal1RefMap h

        -- GEN K2
        k2State <- genKState builder stateVals k1State deltaVal2RefMap tPlusHHalf h $ \(sv, k1v) -> do
            i <- buildFMul builder k1v (constDouble 0.5) ""
            buildFAdd builder sv i "sv"

        -- GEN K3 (same as K2)
        k3State <- genKState builder stateVals k2State deltaVal3RefMap tPlusHHalf h $ \(sv, k2v) -> do
            i <- buildFMul builder k2v (constDouble 0.5) ""
            buildFAdd builder sv i "sv"

        -- GEN K4 (similar to K3)
        k4State <- genKState builder stateVals k3State deltaVal4RefMap tPlusH h $ \(sv, k3v) -> do
            buildFAdd builder sv k3v "sv"

        -- update the states/calc avg. dy and adjust y for all SimOps
        let idMap = Map.fromList $ zip (OrdMap.keys stateValRefMap) [0..]
        forM_ simOps $ updateState builder k1State k2State k3State k4State idMap
        return ()
      where
        -- | generate each of the k vals for RK4
        genKState :: Builder -> [Value] -> [Value] -> ParamMap -> Value -> Value -> ((Value, Value) -> IO Value) -> GenM [Value]
        genKState builder stateVals inKState deltaRefs timeDelta h stateF = do
            -- generate the modified statevals
            stateVals' <- liftIO $ forM (zip stateVals inKState) stateF
            -- call the loop func f(t,y')
            callRHSF builder timeDelta stateVals' deltaRefs
            -- k' = h * f(t,y')
            kState' <- multByTimeStep builder deltaRefs h
            return kState'

        -- | wrapper to call the acutal model rhs function f(t,y)
        callRHSF :: Builder -> Value -> [Value] -> ParamMap -> GenM Value
        callRHSF builder timeVal stateVals deltaValRefMap = liftIO $
            buildCall builder rhsF (timeVal : stateVals  ++ (OrdMap.elems deltaValRefMap)) ""

        -- | multiply a vector of refs by the timestep, i.e. h*f(t,y)
        multByTimeStep :: Builder -> ParamMap -> Value -> GenM [Value]
        multByTimeStep builder deltaValRefMap timeStep = liftIO $ forM (OrdMap.elems deltaValRefMap) $ \deltaRef ->
            withPtrVal builder deltaRef $ \deltaVal -> buildFMul builder deltaVal timeStep "deltaTime"

        -- | get & updatestate val using RK4 algo for y' = y + 1/6*(k1 + 2*k2 + 2*k3 + k4)
        updateState :: Builder -> [Value] -> [Value] -> [Value] -> [Value] -> Map.Map Id Int -> SimOps -> GenM ()
        updateState builder k1State k2State k3State k4State idMap (Ode i dV) = liftIO $
            updatePtrVal builder (stateValRefMap OrdMap.! i) $ \stateVal -> do
                let idx = idMap Map.! i
                -- muls -- TODO - replace muls with adds?
                kTmpMul <- buildFMul builder (k2State !! idx) (constDouble 2.0) "kTmpMul"
                kTmpMul1 <- buildFMul builder (k3State !! idx) (constDouble 2.0) "kTmpMul1"
                -- adds
                kTmpAdd <- buildFAdd builder (k1State !! idx) kTmpMul "kTmpAdd"
                kTmpAdd1 <- buildFAdd builder kTmpAdd kTmpMul1 "kTmpAdd1"
                kTmpAdd2 <- buildFAdd builder (k4State !! idx) kTmpAdd1 "kTmpAdd2"
                -- update state val
                kTmpTotal  <- buildFDiv builder kTmpAdd2 (constDouble $ 6.0) "kTmpTotal"
                buildFAdd builder stateVal kTmpTotal "newState"


-- | Generate the infrastrcutre for the internal solvers, i.e. constant time-step, explicit solvers,
--  including much of the machinary to setup variables, write to disk, etc.
genModelSolver :: CF.Module -> LLVM.Value -> LLVM.Value -> GenM LLVM.Value
genModelSolver CF.Module{..} initsF rhsF = do
    (curFunc, builder) <- genFunction  "modelSolver" voidType []
    -- need external linkage to generate a aot executable
    liftIO $ setLinkage curFunc ExternalLinkage
    _ <- liftIO $ addFuncAttributes curFunc [NoUnwindAttribute]-- [NoInlineAttribute, NoUnwindAttribute]
    GenState {libOps, llvmMod, simParams} <- get
    -- call the startSim func
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeInit") [] ""
    fileStrPtr  <- liftIO $ buildGlobalStringPtr builder (L.get Sys.lFilename simParams) "simFilename"
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeStartSim") [fileStrPtr, constInt64 $ outDataSize] ""

    -- choose the solver (and create the vals)
    -- if SDE, then must use EulerM, else choose FEuler or RK4 depending on SimParams
    solver <- case simType of
        CF.SimSDE   -> MkSolver <$> (genVals $ (Map.keys initVals) :: GenM EulerMSolver)
        CF.SimODE   -> case (L.get Sys.lSolver simParams) of
            Sys.FEuler  -> MkSolver <$> (genVals $ (Map.keys initVals) :: GenM EulerSolver)
            Sys.RK4     -> MkSolver <$> (genVals $ (Map.keys initVals) :: GenM RK4Solver)

    -- create mutable sim params (static sim params embeedded as constants)
    simParamVs@(curPeriodRef, curLoopRef, curTimeRef, outStateArray) <- createSimParams outDataSize

    -- call the init funcs
    _ <- liftIO $ withPtrVal builder curTimeRef $ \curTime -> do
        buildCall builder initsF (curTime : OrdMap.elems (getStateVals solver)) ""
    -- write initial data
    writeOutData outStateArray curTimeRef $ OrdMap.elems (getStateVals solver)

    -- create the main solver loop
    doWhileStmt builder curFunc
        -- doBody
        (\builder ->  createSolverLoopBody rhsF solver simParamVs >> (liftIO $ buildNoOp builder))
        -- doCond
        (\builder _ ->  do
            liftIO $ withPtrVal builder curTimeRef $ \curTime -> do
                liftIO $ buildFCmp builder FPOLT curTime (constDouble $ Sys.calcAdjustedStopTime simParams) "bWhileTime")

    -- end sim
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeStopSim") [] ""
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeShutdown") [] ""
    -- return void
    r <- liftIO $ buildRetVoid builder
    liftIO $ disposeBuilder builder
    return curFunc
  where
    outDataSize = Map.size initVals

    -- | Create the main body of the solver loop
    createSolverLoopBody  :: (OdeSolver a) => LLVM.Value -> a -> (LLVM.Value, LLVM.Value, LLVM.Value, LLVM.Value) -> GenM ()
    createSolverLoopBody  rhsF solver (curPeriodRef, curLoopRef, curTimeRef, outStateArray) = do
        GenState {builder, curFunc, simParams} <- get
        let stateValRefMap = getStateVals solver
        -- inc loop counter & calc the time
        liftIO $ updatePtrVal builder curLoopRef (\curLoop -> buildAdd builder curLoop (constInt64 1) "incCurLoop")
        liftIO $ withPtrVal builder curLoopRef $ \ curLoop -> do
            curLoop' <- buildUIToFP builder curLoop doubleType "convDouble"
            timeDelta <- buildFMul builder curLoop' (constDouble $ L.get Sys.lTimestep simParams) "timeDelta"
            curTime <- buildFAdd builder timeDelta (constDouble $ L.get Sys.lStartTime simParams) "curTime"
            buildStore builder curTime curTimeRef

        -- call the solver
        genSolver solver rhsF curTimeRef simOps

        bWriteOut <- liftIO $ withPtrVal builder curPeriodRef $ \curPeriod -> do
            buildICmp builder IntEQ curPeriod (constInt64 $ Sys.calcOutputInterval simParams) "bWriteOut"

        -- check period and writeOutData if needed
        _ <- ifStmt builder curFunc bWriteOut
            -- ifTrue
            (\builder -> do
                _ <- writeOutData outStateArray curTimeRef $ OrdMap.elems (getStateVals solver)
                liftIO $ buildStore builder (constInt64 1) curPeriodRef)
            -- ifFalse
            (\builder -> do
                liftIO $ updatePtrVal builder curPeriodRef $ \curPeriod -> buildAdd builder curPeriod (constInt64 1) "incCurPeriod"
                liftIO $ buildNoOp builder)

        return ()



-- | Generate the function that calculates the SSA based on the list of RREs
-- modelSolver(void)
genSSASolver :: CF.Module -> LLVM.Value -> GenM LLVM.Value
genSSASolver CF.Module{..} initsF = do
    (curFunc, builder) <- genFunction  "modelSolver" voidType []
    -- need external linkage to generate a aot executable
    liftIO $ setLinkage curFunc ExternalLinkage
    _ <- liftIO $ addFuncAttributes curFunc [NoUnwindAttribute]-- [NoInlineAttribute, NoUnwindAttribute]
    GenState {libOps, llvmMod, simParams} <- get
    -- call the startSim func
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeInit") [] ""
    fileStrPtr  <- liftIO $ buildGlobalStringPtr builder (L.get Sys.lFilename simParams) "simFilename"
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeStartSim") [fileStrPtr, constInt64 outDataSize] ""

    -- create mutable sim params (static sim params embeedded as constants)
    simParamVs@(curPeriodRef, curLoopRef, curTimeRef, outStateArray) <- createSimParams outDataSize
    -- set loopCount to 1
    _ <- liftIO $ buildStore builder (constInt64 1) curLoopRef

    -- create and init state vals
    stateValRefMap <- createVals (Map.keys initVals) "StateRef"
    -- call the init funcs
    _ <- liftIO $ withPtrVal builder curTimeRef $ \curTime -> do
        buildCall builder initsF (curTime : OrdMap.elems stateValRefMap) ""
    -- write initial data
    writeOutData outStateArray curTimeRef $ OrdMap.elems stateValRefMap

    -- setup init sumprops
    sumPropRef <- liftIO $ buildAlloca builder doubleType "sumProp"
    sumPropensities sumPropRef stateValRefMap

    -- setup nextOutput val
    nextOutputInitial <- liftIO $ buildFAdd builder (constDouble $ L.get Sys.lStartTime simParams)
                                           (constDouble $ L.get Sys.lOutputPeriod simParams) "nextOutputInitial"
    nextOutputRef <- liftIO $ buildAllocaWithInit builder nextOutputInitial doubleType "nextOutput"

    -- create the main solver loop - use while stmt
    whileStmt builder curFunc
        -- whileCond
        (\builder ->
            liftIO $ withPtrVal builder curTimeRef $ \curTime ->
            liftIO $ withPtrVal builder sumPropRef $ \sumProp -> do
                -- dbgStr <- buildGlobalStringPtr builder "While conds - sumProp : %g, time : %g\n" "dbgStr"
                -- _ <- buildCall builder (libOps Map.! "printf") [dbgStr, sumProp, curTime] ""
                cond1 <- buildFCmp builder FPOGT sumProp constZero "bSumPropPos"
                cond2 <- buildFCmp builder FPOLT curTime (constDouble $ Sys.calcAdjustedStopTime simParams) "bWhileTime"
                buildAnd builder cond1 cond2 "bAllConds")
        -- whileBody
        (\builder ->  createSolverLoopBody sumPropRef nextOutputRef stateValRefMap simParamVs >> (liftIO $ buildNoOp builder))

    -- end sim
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeStopSim") [] ""
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeShutdown") [] ""
    -- return void
    r <- liftIO $ buildRetVoid builder
    liftIO $ disposeBuilder builder
    return curFunc

  where
    outDataSize = Map.size initVals

    -- | Create the main body of the SSA solver loop
    createSolverLoopBody  :: LLVM.Value -> LLVM.Value -> ParamMap -> (LLVM.Value, LLVM.Value, LLVM.Value, LLVM.Value) -> GenM ()
    createSolverLoopBody  sumPropRef nextOutputRef stateValRefMap (curPeriodRef, curLoopRef, curTimeRef, outStateArray) = do
        GenState {builder, curFunc, simParams} <- get
        tau <- chooseTimestep sumPropRef

        -- choose and trigger reaction
        chooseTriggerReaction sumPropRef stateValRefMap

        -- update time
        liftIO $ updatePtrVal builder curTimeRef $ \curTime ->
            buildFAdd builder curTime tau "incTime"

        -- write output
        bWriteOut <-    liftIO $ withPtrVal builder curTimeRef $ \curTime ->
                        liftIO $ withPtrVal builder nextOutputRef $ \nextOutput ->
                            buildFCmp builder FPOGE curTime nextOutput "bWriteOut"

        -- check period and writeOutData if needed
        _ <- ifStmt builder curFunc bWriteOut
            -- ifTrue
            (\builder -> do
                -- update loop ptr and next val
                liftIO $ updatePtrVal builder curLoopRef $ \curLoop -> do
                    curLoop' <- buildAdd builder curLoop (constInt64 1) "incLoop"
                    updatePtrVal builder nextOutputRef $ \nextOutput -> do
                        curLoop'' <- buildUIToFP builder curLoop' doubleType "cast"
                        buildFMul builder curLoop'' (constDouble $ L.get Sys.lOutputPeriod simParams) "nextOutput"
                    return curLoop'
                -- write data
                writeOutData outStateArray curTimeRef $ OrdMap.elems stateValRefMap
                liftIO $ buildNoOp builder)

            -- ifFalse
            (\builder -> liftIO $ buildNoOp builder)

        -- update sumprops
        sumPropensities sumPropRef stateValRefMap

        return ()

    chooseTimestep :: LLVM.Value -> GenM (LLVM.Value)
    chooseTimestep sumPropRef = do
        GenState {builder, libOps, mathOps} <- get
        liftIO $ do
            ts1 <- buildCall builder (libOps Map.! "OdeRandUniform") [] "tsRand"
            ts2 <- buildCall builder (mathOps Map.! AC.Log) [ts1] "tsLog"
            ts3 <- withPtrVal builder sumPropRef $ \sumProp ->
                buildFDiv builder constOne sumProp "tsRecip"
            ts4 <- buildFNeg builder ts3 "tsNeg"
            buildFMul builder ts4 ts2 "tau"

    -- | we choose and trigger the reaction in a single block of code as is easier, already have access to the params within the bb
    chooseTriggerReaction :: LLVM.Value -> ParamMap -> GenM ()
    chooseTriggerReaction sumPropRef stateValsRefMap = do
        GenState {builder, libOps, curFunc} <- get

        liftIO $ do
            r2 <- buildCall builder (libOps Map.! "OdeRandUniform") [] "trRand"
            endProp <- withPtrVal builder sumPropRef $ \sumProp ->
                buildFMul builder r2 sumProp "endProp"


            nextBB <- appendBasicBlock curFunc "next.TR"
            endBB <- appendBasicBlock curFunc "end.TR"
            startBB <- appendBasicBlock curFunc "start.TR"
            -- startup foldlM with branch to first
            buildBr builder startBB
            positionAtEnd builder startBB

            (curProp', nextBB') <- DF.foldlM (f builder curFunc endProp endBB) (constZero, nextBB) simOps

            -- pos at nextBB' and jump to final bb - clean up foldM
            buildBr builder endBB
            positionAtEnd builder nextBB'
            buildBr builder endBB

            positionAtEnd builder endBB

            return ()

      where
        f builder curFunc endProp endBB (curProp, nextBB) rreOp@(CF.Rre srcs dests rate) = do
            curProp' <- calcPropensity builder stateValsRefMap curProp rreOp

            -- now build branch to trigger the reaction
            triggerBB <- appendBasicBlock curFunc "trigger.TR"
            cmpProp <- buildFCmp builder FPOGT curProp' endProp "cmpProps"
            buildCondBr builder cmpProp triggerBB nextBB

            -- build triggerbb
            positionAtEnd builder triggerBB
            mapM_ (f' buildFSub) srcs
            mapM_ (f' buildFAdd) dests

            buildBr builder endBB

            -- repos the builder and pass to next BB
            positionAtEnd builder nextBB
            nextBB' <- appendBasicBlock curFunc "next.TR"
            return (curProp', nextBB')
              where
                f' fOp (i, vId) = do
                    updatePtrVal builder (stateValsRefMap OrdMap.! vId) $ \v ->
                        fOp builder v (constDouble . fromIntegral $ i) "updatePop"



    -- | Iterate over the RREs and both calculate the props and sum them up
    sumPropensities :: LLVM.Value -> ParamMap -> GenM (LLVM.Value)
    sumPropensities sumPropRef stateValsRefMap = do
        GenState {builder, simParams, llvmMod} <- get
        sumProp <- liftIO $ DF.foldlM (calcPropensity builder stateValsRefMap) constZero simOps
        liftIO $ buildStore builder sumProp sumPropRef

    calcPropensity :: Builder -> ParamMap -> LLVM.Value -> SimOps -> IO (LLVM.Value)
    calcPropensity builder stateValsRefMap curSumProp (CF.Rre srcs _ rate) = do
        reactionProp <- DF.foldlM calcReactionProp constOne srcs
        reactionProp' <- buildFMul builder reactionProp (constDouble rate) "prop3"
        buildFAdd builder reactionProp' curSumProp "sumProp"
      where
        calcReactionProp curReactionProp (i, vId) = do
            -- load the val
            v <- buildLoad builder (stateValsRefMap OrdMap.! vId) "loadState"
            prop1 <- buildFMul builder (constDouble . fromIntegral $ i) v "prop1"
            buildFMul builder prop1 curReactionProp "prop2"






-- | create the global variables - to hold STATE and DELTA
createVals :: [Id] -> String -> GenM ParamMap
createVals ids suffix = foldM createVal OrdMap.empty ids
  where
    createVal idMap i = do
        GenState {builder, llvmMod} <- get
        llV <- liftIO $ addGlobalWithInit llvmMod (constDouble 0.0) doubleType False (getName i)
        liftIO $ setLinkage llV PrivateLinkage
        return $ OrdMap.insert i llV idMap
    getName i = (getValidIdName i) ++ suffix

-- | create most (mutable) sim params
createSimParams :: Int -> GenM (LLVM.Value, LLVM.Value, LLVM.Value, LLVM.Value)
createSimParams outDataSize = do
    GenState {builder, simParams, llvmMod} <- get
    curPeriodRef <- liftIO $ addGlobalWithInit llvmMod (constInt64 1) int64Type False "simCurPeriod"
    curLoopRef <- liftIO $ addGlobalWithInit llvmMod (constInt64 0) int64Type False "simCurLoop"
    -- set inital time
    curTimeRef <- liftIO $ addGlobalWithInit llvmMod (constDouble $ L.get Sys.lStartTime simParams) doubleType False "simCurTime"
    -- set output vector
    -- outDataRef <- liftIO $ buildAlloca builder (LFFI.arrayType doubleType $ fromIntegral outDataSize) "simOutData"
    initOutData <- liftIO $ constArray doubleType $ replicate outDataSize (constDouble 0.0)
    outStateArray <- liftIO $ addGlobalWithInit llvmMod initOutData (LFFI.arrayType doubleType $ fromIntegral outDataSize) False "simOutData"
    -- set linkages
    liftIO $ mapM_ (\v -> setLinkage v PrivateLinkage) [curPeriodRef, curLoopRef, curTimeRef, outStateArray]
    return (curPeriodRef, curLoopRef, curTimeRef, outStateArray)

-- | Write the current time and STATE to disk
writeOutData :: LLVM.Value -> LLVM.Value -> [LLVM.Value] -> GenM ()
writeOutData outStateArray curTimeRef stateValRefs = do
    GenState {builder, libOps} <- get
    -- fill the state array
    forM_ (zip [0..] stateValRefs) $ \(i, ptrVal) -> liftIO $ do
        outV <- buildInBoundsGEP builder outStateArray [constInt64 0, constInt64 i] $ "stateArrayPtr" ++ (show i)
        withPtrVal builder ptrVal $ \loadV -> liftIO $ buildStore builder loadV outV
    -- write to output func
    simStateArrayPtr <- liftIO $ buildInBoundsGEP builder outStateArray [constInt64 0, constInt64 0] $ "storeOutPtr"
    liftIO $ withPtrVal builder curTimeRef $ \curTimeVal ->
        buildCall builder (libOps Map.! "OdeWriteState") [curTimeVal, simStateArrayPtr] ""
    -- liftIO $ setInstructionCallConv callInst Fast
    return ()
