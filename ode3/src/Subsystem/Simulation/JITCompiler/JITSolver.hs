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
genModelSolver, genAOTMain, genFFIParams, genFFIModelInitials, genFFIModelLoop
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


-- | Write sim param constant to global vals within the module - look at OdeModel.h for interface details
genFFIParams numParams = do
    GenState {builder, simParams, llvmMod} <- get
    let Sys.SimParams{..} = simParams
    liftIO $ addGlobalWithInit llvmMod (constDouble $ _startTime) doubleType True "OdeParamStartTime"
    liftIO $ addGlobalWithInit llvmMod (constDouble $ _stopTime) doubleType True "OdeParamStopTime"
    liftIO $ addGlobalWithInit llvmMod (constDouble $ _timestep) doubleType True "OdeParamTimestep"
    liftIO $ addGlobalWithInit llvmMod (constDouble $ Sys.calcAdjustedStopTime simParams) doubleType True "OdeParamAdjustedStopTime"

    -- adaptive params
    liftIO $ addGlobalWithInit llvmMod (constDouble $ _maxTimestep) doubleType True "OdeParamMaxTimestep"
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

    -- model size
    liftIO $ addGlobalWithInit llvmMod (constInt64 $ numParams) int64Type True "OdeParamStateSize"


genFFIModelInitials initsF numParams = do
    (curFunc, builder) <- genFunction "OdeModelInitials" voidType createArgsList

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

genFFIModelLoop loopF numParams = do
    (curFunc, builder) <- genFunction "OdeModelLoop" voidType createArgsList

    (curTimeVal : stateArrayRef : deltaArrayRef : []) <- liftIO $ LLVM.getParams curFunc

    -- use GEP to build list of ptrs into stateVals array, need to deref each one
    stateArgs <- liftIO $ forM [0..(numParams-1)] $ \idx -> do
        stateRef <- buildInBoundsGEP builder stateArrayRef [constInt64 0, constInt64 idx] "stateValRef"
        buildLoad builder stateRef "stateVal"

    -- use GEP to build list of ptrs into deltaVals array, can pass direct into modelLoop
    deltaArgs <- liftIO $ forM [0..(numParams-1)] $ \idx ->
        buildInBoundsGEP builder deltaArrayRef [constInt64 0, constInt64 idx] "deltaValRef"

    -- call the internal inits functions
    liftIO $ buildCall builder loopF (curTimeVal : stateArgs ++ deltaArgs) ""
    liftIO $ buildRetVoid builder
    return ()

    return ()
  where
    -- create the input args
    createArgsList = [doubleType, pointerType (arrayType doubleType (fromIntegral numParams)) 0
                                , pointerType (arrayType doubleType (fromIntegral numParams)) 0]


-- A solver class for abstracting over the differing code-gen requriements
class OdeSolver a where
    genVals :: [Id] -> GenM a
    genSolver :: a -> LLVM.Value -> LLVM.Value -> [SimOps] -> GenM ()
    getStateVals :: a -> ParamMap

-- Existential wrapper for solver
data Solver :: * where
    MkSolver :: OdeSolver a => a -> Solver

instance OdeSolver Solver where
    genVals ids = genVals ids
    genSolver (MkSolver s) loopF curTimeRef = genSolver s loopF curTimeRef
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

    genSolver (EulerSolver stateValRefMap deltaValRefMap) loopF curTimeRef simOps = do
        GenState {builder, curFunc, simParams} <- get
        -- call the modelLoop func
        stateVals <- mapM (\v -> liftIO $ buildLoad builder v "odeValx") $ OrdMap.elems stateValRefMap
        _ <- liftIO $ withPtrVal builder curTimeRef $ \curTime -> do
            buildCall builder loopF (curTime : stateVals  ++ OrdMap.elems deltaValRefMap) ""

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
    genSolver (RK4Solver stateValRefMap deltaVal1RefMap deltaVal2RefMap deltaVal3RefMap deltaVal4RefMap) loopF curTimeRef simOps = do
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
        -- call the modelLoop func manually
        callLoopF builder t stateVals deltaVal1RefMap
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
            callLoopF builder timeDelta stateVals' deltaRefs
            -- k' = h * f(t,y')
            kState' <- multByTimeStep builder deltaRefs h
            return kState'

        -- | wrapper to call the acutal mdoel loop function f(t,y)
        callLoopF :: Builder -> Value -> [Value] -> ParamMap -> GenM Value
        callLoopF builder timeVal stateVals deltaValRefMap = liftIO $
            buildCall builder loopF (timeVal : stateVals  ++ (OrdMap.elems deltaValRefMap)) ""

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


-- | Generate the forward euler solver, also including much of the machinary to setup variables, write to disk, etc.
genModelSolver :: CF.Module -> LLVM.Value -> LLVM.Value -> GenM LLVM.Value
genModelSolver CF.Module{..} initsF loopF = do
    (curFunc, builder) <- genFunction  "modelSolver" voidType []
    -- need external linkage to generate a aot executable
    liftIO $ setLinkage curFunc ExternalLinkage
    _ <- liftIO $ addFuncAttributes curFunc [NoUnwindAttribute]-- [NoInlineAttribute, NoUnwindAttribute]
    GenState {libOps, llvmMod, simParams} <- get
    -- call the startSim func
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeInit") [] ""
    fileStr <- liftIO $ buildGlobalString builder (L.get Sys.lFilename simParams) "simFilename"
    fileStrPtr <- liftIO $ buildInBoundsGEP builder fileStr [constInt64 0, constInt64 0] ""
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeStartSim") [fileStrPtr, constInt64 $ OrdMap.size initExprs] ""

    -- create the vals (and indirectly choose the solver)
    solver <- case (L.get Sys.lSolver simParams) of
        Sys.FEuler  -> MkSolver <$> (genVals $ (OrdMap.keys initExprs) :: GenM EulerSolver)
        Sys.RK4     -> MkSolver <$> (genVals $ (OrdMap.keys initExprs) :: GenM RK4Solver)

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
        (\builder ->  createSolverLoopBody loopF solver simParamVs >> (liftIO $ buildNoOp builder))
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
    outDataSize = OrdMap.size initExprs + 1

    -- | Create the main body of the solver loop
    createSolverLoopBody  :: (OdeSolver a) => LLVM.Value -> a -> (LLVM.Value, LLVM.Value, LLVM.Value, LLVM.Value) -> GenM ()
    createSolverLoopBody  loopF solver (curPeriodRef, curLoopRef, curTimeRef, outStateArray) = do
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
        genSolver solver loopF curTimeRef simOps

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

-- | create the global variables - to hold STATE and DELTA
createVals :: [Id] -> String -> GenM (OrdMap.OrdMap Id LLVM.Value)
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
