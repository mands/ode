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
genModelSolver, genAOTMain
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
-- a standalone object file

-- | A stub main function used for AOT compilation
genAOTMain :: LLVM.Value -> GenM ()
genAOTMain simF = do
    (curFunc, builder) <- genFunction "main" int32Type []
    -- call the sim func
    _ <- liftIO $ buildCall builder simF [] ""
    liftIO $ buildRet builder $ constInt32 0
    return ()


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
        -- call the modelLoop func
        callLoopF builder t stateVals deltaVal1RefMap
        k1State <- multDeltasTimeStep builder deltaVal1RefMap h

        -- GEN K2
        stateVals2 <- liftIO $ forM (zip stateVals k1State) $ \(sv, k1v) -> do
            i <- buildFMul builder k1v (constDouble 0.5) ""
            buildFAdd builder sv i "sv"

        callLoopF builder tPlusHHalf stateVals2 deltaVal2RefMap
        k2State <- multDeltasTimeStep builder deltaVal2RefMap h

        -- GEN K3 (same as K2)
        stateVals3 <- liftIO $ forM (zip stateVals k2State) $ \(sv, k2v) -> do
            i <- buildFMul builder k2v (constDouble 0.5) ""
            buildFAdd builder sv i "sv"

        callLoopF builder tPlusHHalf stateVals3 deltaVal3RefMap
        k3State <- multDeltasTimeStep builder deltaVal3RefMap h

        -- GEN K4 (similar to K3)
        stateVals4 <- liftIO $ forM (zip stateVals k3State) $ \(sv, k3v) -> do
            buildFAdd builder sv k3v "sv"

        callLoopF builder tPlusH stateVals3 deltaVal3RefMap
        k4State <- multDeltasTimeStep builder deltaVal3RefMap h

        -- update the states/calc avg. dy and adjust y for all SimOps
        let idMap = Map.fromList $ zip (OrdMap.keys stateValRefMap) [0..]
        liftIO $ forM_ simOps $ updateState builder k1State k2State k3State k4State idMap
        return ()
      where
        callLoopF builder timeVal stateVals deltaValRefMap = liftIO $
            buildCall builder loopF (timeVal : stateVals  ++ (OrdMap.elems deltaValRefMap)) ""

        multDeltasTimeStep builder deltaValRefMap timeStep = liftIO $ mapM multTimeStep (OrdMap.elems deltaValRefMap)
          where
            multTimeStep deltaRef = withPtrVal builder deltaRef $ \deltaVal -> buildFMul builder deltaVal timeStep "deltaTime"

        -- updateState :: Builder -> Sys.SimParams -> SimOps -> IO ()
        updateState builder k1State k2State k3State k4State idMap (Ode i dV) = do
            -- get & updatestate val
            updatePtrVal builder (stateValRefMap OrdMap.! i) $ \stateVal -> do
                let idx = idMap Map.! i
                -- y' = y + 1/6*(k1 + 2*k2 + 2*k3 + k4)
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
    _ <- liftIO $ buildCall builder (libOps Map.! "init") [] ""
    fileStr <- liftIO $ buildGlobalString builder (L.get Sys.lFilename simParams) "simFilename"
    fileStrPtr <- liftIO $ buildInBoundsGEP builder fileStr [constInt64 0, constInt64 0] ""
    _ <- liftIO $ buildCall builder (libOps Map.! "startSim") [fileStrPtr, constInt64 $ OrdMap.size initExprs + 1] ""

    -- create the vals (and indirectly choose the solver)
    solver <- case (L.get Sys.lSolver simParams) of
        Sys.FEuler  -> MkSolver <$> (genVals $ (OrdMap.keys initExprs) :: GenM EulerSolver)
        Sys.RK4     -> MkSolver <$> (genVals $ (OrdMap.keys initExprs) :: GenM RK4Solver)

    -- create mutable sim params (static sim params embeedded as constants)
    simParamVs@(curPeriodRef, curLoopRef, curTimeRef, outDataRef) <- createSimParams outDataSize

    -- call the init funcs
    _ <- liftIO $ withPtrVal builder curTimeRef $ \curTime -> do
        buildCall builder initsF (curTime : OrdMap.elems (getStateVals solver)) ""
    -- write initial data
    writeOutData outDataSize outDataRef curTimeRef $ OrdMap.elems (getStateVals solver)

    -- create the main solver loop
    doWhileStmt builder curFunc
        -- doBody
        (\builder ->  createSolverLoopBody loopF solver simParamVs >> (liftIO $ buildNoOp builder))
        -- doCond
        (\builder _ ->  do
            liftIO $ withPtrVal builder curTimeRef $ \curTime -> do
                liftIO $ buildFCmp builder FPOLT curTime (constDouble $ L.get Sys.lEndTime simParams) "bWhileTime")

    -- end sim
    _ <- liftIO $ buildCall builder (libOps Map.! "endSim") [] ""
    _ <- liftIO $ buildCall builder (libOps Map.! "shutdown") [] ""
    -- return void
    r <- liftIO $ buildRetVoid builder
    liftIO $ disposeBuilder builder
    return curFunc
  where
    outDataSize = OrdMap.size initExprs + 1

    -- | Create the main body of the solver loop
    createSolverLoopBody  :: (OdeSolver a) => LLVM.Value -> a -> (LLVM.Value, LLVM.Value, LLVM.Value, LLVM.Value) -> GenM ()
    createSolverLoopBody  loopF solver (curPeriodRef, curLoopRef, curTimeRef, outDataRef) = do
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
            buildICmp builder IntEQ curPeriod (constInt64 $ L.get Sys.lOutputPeriod simParams) "bWriteOut"

        -- check period and writeOutData if needed
        _ <- ifStmt builder curFunc bWriteOut
            -- ifTrue
            (\builder -> do
                _ <- writeOutData outDataSize outDataRef curTimeRef $ OrdMap.elems stateValRefMap
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
        llV <- liftIO $ addGlobalWithInit llvmMod (constDouble 0.0) doubleType (getName i)
        liftIO $ setLinkage llV PrivateLinkage
        return $ OrdMap.insert i llV idMap
    getName i = (getValidIdName i) ++ suffix

-- | create most (mutable) sim params
createSimParams :: Int -> GenM (LLVM.Value, LLVM.Value, LLVM.Value, LLVM.Value)
createSimParams outDataSize = do
    GenState {builder, simParams, llvmMod} <- get
    curPeriodRef <- liftIO $ addGlobalWithInit llvmMod (constInt64 1) int64Type "simCurPeriod"
    curLoopRef <- liftIO $ addGlobalWithInit llvmMod (constInt64 0) int64Type "simCurLoop"
    -- set inital time
    curTimeRef <- liftIO $ addGlobalWithInit llvmMod (constDouble $ L.get Sys.lStartTime simParams) doubleType "simCurTime"
    -- set output vector
    -- outDataRef <- liftIO $ buildAlloca builder (LFFI.arrayType doubleType $ fromIntegral outDataSize) "simOutData"
    initOutData <- liftIO $ constArray doubleType $ replicate outDataSize (constDouble 0.0)
    outDataRef <- liftIO $ addGlobalWithInit llvmMod initOutData (LFFI.arrayType doubleType $ fromIntegral outDataSize) "simOutData"
    -- set linkages
    liftIO $ mapM_ (\v -> setLinkage v PrivateLinkage) [curPeriodRef, curLoopRef, curTimeRef, outDataRef]
    return (curPeriodRef, curLoopRef, curTimeRef, outDataRef)

-- | Write the current time and STATE to disk
writeOutData :: Int -> LLVM.Value -> LLVM.Value -> [LLVM.Value] -> GenM ()
writeOutData outDataSize simOutData curTimeRef stateValRefs = do
    GenState {builder, libOps} <- get
    -- fill the output array (inc curTime)
    forM_ (zip [0..] (curTimeRef:stateValRefs)) $ \(i, ptrVal) -> do
        outV <- liftIO $ buildInBoundsGEP builder simOutData [constInt64 0, constInt64 i] $ "storeOutPtr" ++ (show i)
        liftIO $ withPtrVal builder ptrVal $ \loadV -> liftIO $ buildStore builder loadV outV
        return ()
    -- write to output func
    simOutDataPtr <- liftIO $ buildInBoundsGEP builder simOutData [constInt64 0, constInt64 0] $ "storeOutPtr"
    callInst <- liftIO $ buildCall builder (libOps Map.! "writeDbls") [simOutDataPtr, constInt64 outDataSize] ""
    -- liftIO $ setInstructionCallConv callInst Fast
    return ()
