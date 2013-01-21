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
-- | The main interface to the internal and external solvers
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Subsystem.Simulation.JITCompiler.JITSolver (
genDiffSolver, genSSASolver, genAOTMain, genFFIParams, genFFIModelInitials, genFFIModelRHS,
-- useful combinators
createSimParams, writeOutData
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
import Subsystem.Simulation.JITCompiler.JITDiffSolvers


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
-- Elements are stored as read-only data within object file
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
    case _modelType of -- hardcode the enum
        Sys.Stiff       -> liftIO $ addGlobalWithInit llvmMod (constInt32 $ 0) int32Type True "OdeParamModelType"
        Sys.NonStiff    -> liftIO $ addGlobalWithInit llvmMod (constInt32 $ 1) int32Type True "OdeParamModelType"

    -- output params
    liftIO $ addGlobalWithInit llvmMod (constDouble $ _outputPeriod) doubleType True "OdeParamPeriod"
    liftIO $ addGlobalWithInit llvmMod (constInt64 $ Sys.calcOutputInterval simParams) int64Type True "OdeParamPeriodInterval"

    -- HACK to build global string outside of a func (as buildGlobalString fails)
    liftIO $ addGlobalWithInit llvmMod (constString _filename False) (arrayType int8Type (fromIntegral $ length _filename + 1)) True "OdeParamOutput"
    -- liftIO $ buildGlobalString builder "test" "test" -- (Sys._filename  simParams) "OdeParamOutput"

    -- sim type - hardcode the enum
    case simType of
        CF.SimODE   -> liftIO $ addGlobalWithInit llvmMod (constInt32 $ 0) int32Type True "OdeParamSimType"
        CF.SimSDE   -> liftIO $ addGlobalWithInit llvmMod (constInt32 $ 1) int32Type True "OdeParamSimType"
        CF.SimRRE   -> liftIO $ addGlobalWithInit llvmMod (constInt32 $ 2) int32Type True "OdeParamSimType"
        CF.SimHybrid   -> liftIO $ addGlobalWithInit llvmMod (constInt32 $ 3) int32Type True "OdeParamSimType"

    -- model size
    liftIO $ addGlobalWithInit llvmMod (constInt64 $ numParams) int64Type True "OdeParamStateSize"
    return ()


-- | C-compatible wrapper for the initial values function
-- OdeModelInitials(double, STATE*)
genFFIModelInitials :: CF.Module -> GenM ()
genFFIModelInitials CF.Module{initVals} = do
    (curFunc, builder) <- genFunction "OdeModelInitials" voidType createArgsList
    liftIO $ setFuncParam curFunc 1 [NoAliasAttribute, NoCaptureAttribute]

    (curTimeVal : stateArrayRef : []) <- liftIO $ LLVM.getParams curFunc

    -- build the LocalMap and gen the initial vals
    stateRefMap <- foldM (buildArrayRefMap builder stateArrayRef) Map.empty (zip (Map.keys initVals) [0..])
    genModelInitials initVals stateRefMap

    liftIO $ buildRetVoid builder
    return ()
  where
    numParams = Map.size initVals
    -- create the input args
    createArgsList = [doubleType, pointerType (arrayType doubleType (fromIntegral numParams)) 0]


-- | C-compatible wrapper for the RHS function
-- takes severl ptrs->arrays for the state/detla/wiener vals
-- OdeModelRHS(double, STATE*, DELTA*, WEINER*)
genFFIModelRHS :: CF.Module -> GenM ()
genFFIModelRHS CF.Module{..} = do
    (curFunc, builder) <- genFunction "OdeModelRHS" voidType createArgsList
    liftIO $ setFuncParam curFunc 1 [NoAliasAttribute, NoCaptureAttribute]
    liftIO $ setFuncParam curFunc 2 [NoAliasAttribute, NoCaptureAttribute]
    liftIO $ setFuncParam curFunc 3 [NoAliasAttribute, NoCaptureAttribute]

    (curTimeVal : stateArrayRef : deltaArrayRef : wienerArrayRef : []) <- liftIO $ LLVM.getParams curFunc

    -- use GEP to build list of ptrs into stateVals array, need to deref each one
    stateRefMap <- foldM (buildArrayRefMap builder stateArrayRef) Map.empty initValIdxs
    deltaRefMap <- foldM (buildArrayRefMap builder deltaArrayRef) Map.empty initValIdxs
    wienerRefMap <-  if simType == CF.SimSDE
                          then foldM (buildArrayRefMap builder wienerArrayRef) Map.empty initValIdxs
                          else return Map.empty

    -- gen the rhs code
    stateValMap <- loadRefMap stateRefMap
    genModelRHS loopExprs simOps curTimeVal stateValMap deltaRefMap wienerRefMap

    liftIO $ buildRetVoid builder
    return ()
  where
    numParams = Map.size initVals
    initValIdxs = (zip (Map.keys initVals) [0..])
    -- create the input args
    createArgsList = [doubleType, pointerType (arrayType doubleType (fromIntegral numParams)) 0
                                , pointerType (arrayType doubleType (fromIntegral numParams)) 0
                                , pointerType (arrayType doubleType (fromIntegral numParams)) 0]

-- use GEP to build list of ptrs into stateVals array, can pass direct into modelInits
buildArrayRefMap builder stateArrayRef refMap (initVal, idx) = liftIO $ do
    ref <- buildInBoundsGEP builder stateArrayRef [constInt64 0, constInt64 idx] "valRef"
    return $ Map.insert initVal ref refMap


-- Internal Solver Interfaces -------------------------------------------------------------------------------------------

-- | Generate the infrastrcutre for the internal solvers for differntial equations
-- , i.e. constant time-step, explicit solvers, including much of the machinary to setup variables, write to disk, etc.
genDiffSolver :: CF.Module -> GenM LLVM.Value
genDiffSolver odeMod@CF.Module{..} = do
    (curFunc, builder) <- genFunction  "modelSolver" voidType []
    -- need external linkage to generate a aot executable
    liftIO $ setLinkage curFunc ExternalLinkage
    _ <- liftIO $ addFuncAttributes curFunc [NoUnwindAttribute] -- ,NoInlineAttribute]
    GenState {libOps, llvmMod, simParams} <- get
    -- call the startSim func
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeInit") [] ""
    fileStrPtr  <- liftIO $ buildGlobalStringPtr builder (L.get Sys.lFilename simParams) "simFilename"
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeStartSim") [fileStrPtr, constInt64 $ outDataSize] ""

    -- choose the solver (and create the vals) depending on SimParams
    trace' [MkSB simType, MkSB $ L.get Sys.lOdeSolver simParams, MkSB $ L.get Sys.lSdeSolver simParams] "Sim params" $ return ()
    solver <- case simType of
        CF.SimSDE   -> case (L.get Sys.lSdeSolver simParams) of
            Sys.EM          -> MkSolver <$> (genVals (Map.keys initVals) simOps :: GenM EulerMSolver)
            Sys.ProjEM      -> MkSolver <$> (genVals (Map.keys initVals) simOps :: GenM ProjSolver)
        CF.SimODE   -> case (L.get Sys.lOdeSolver simParams) of
            Sys.FEuler      -> MkSolver <$> (genVals (Map.keys initVals) simOps :: GenM EulerSolver)
            Sys.RK4         -> MkSolver <$> (genVals (Map.keys initVals) simOps :: GenM RK4Solver)

    -- create mutable sim params (static sim params embeedded as constants)
    simParamVs@(curPeriodRef, curLoopRef, curTimeRef, outStateArray) <- createSimParams outDataSize

    -- setup and write initial data
    genModelInitials initVals (getStateVals solver)
    writeOutData outStateArray curTimeRef $ Map.elems (getStateVals solver)

    -- create the main solver loop
    doWhileStmt builder curFunc
        -- doBody
        (\builder ->  createSolverLoopBody solver odeMod simParamVs >> (liftIO $ buildNoOp builder))
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
    createSolverLoopBody  :: (OdeSolver a) => a -> CF.Module -> (LLVM.Value, LLVM.Value, LLVM.Value, LLVM.Value) -> GenM ()
    createSolverLoopBody solver odeMod (curPeriodRef, curLoopRef, curTimeRef, outStateArray) = do
        GenState {builder, curFunc, simParams} <- get
        let stateRefMap = getStateVals solver
        -- inc loop counter & calc the time
        liftIO $ updatePtrVal builder curLoopRef (\curLoop -> buildAdd builder curLoop (constInt64 1) "incCurLoop")
        liftIO $ withPtrVal builder curLoopRef $ \ curLoop -> do
            curLoop' <- buildUIToFP builder curLoop doubleType "convDouble"
            timeDelta <- buildFMul builder curLoop' (constDouble $ L.get Sys.lTimestep simParams) "timeDelta"
            curTime <- buildFAdd builder timeDelta (constDouble $ L.get Sys.lStartTime simParams) "curTime"
            buildStore builder curTime curTimeRef

        -- call the solver
        genSolver solver curTimeRef odeMod

        bWriteOut <- liftIO $ withPtrVal builder curPeriodRef $ \curPeriod -> do
            buildICmp builder IntEQ curPeriod (constInt64 $ Sys.calcOutputInterval simParams) "bWriteOut"

        -- check period and writeOutData if needed
        _ <- ifStmt builder curFunc bWriteOut
            -- ifTrue
            (\builder -> do
                _ <- writeOutData outStateArray curTimeRef $ Map.elems (getStateVals solver)
                liftIO $ buildStore builder (constInt64 1) curPeriodRef)
            -- ifFalse
            (\builder -> do
                liftIO $ updatePtrVal builder curPeriodRef $ \curPeriod -> buildAdd builder curPeriod (constInt64 1) "incCurPeriod"
                liftIO $ buildNoOp builder)

        return ()


-- | Generate the solver for simulations RREs using the SSA
genSSASolver :: CF.Module -> GenM LLVM.Value
genSSASolver CF.Module{..} = do
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

    -- create, setup and write initial data
    stateRefMap <- createVals (Map.keys initVals) "StateRef"
    genModelInitials initVals stateRefMap
    writeOutData outStateArray curTimeRef $ Map.elems stateRefMap

    -- eval expressions and setup init sumprops
    evalExprs curTimeRef stateRefMap
    sumPropRef <- liftIO $ buildAlloca builder doubleType "sumProp"
    sumPropensities sumPropRef stateRefMap

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
                -- debugStmt "While conds - sumProp : %g, time : %g\n" [dbgStr, sumProp, curTime]
                cond1 <- buildFCmp builder FPOGT sumProp constZero "bSumPropPos"
                cond2 <- buildFCmp builder FPOLT curTime (constDouble $ Sys.calcAdjustedStopTime simParams) "bWhileTime"
                buildAnd builder cond1 cond2 "bAllConds")
        -- whileBody
        (\builder ->  createSolverLoopBody sumPropRef nextOutputRef stateRefMap simParamVs >> (liftIO $ buildNoOp builder))

    -- end sim
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeStopSim") [] ""
    _ <- liftIO $ buildCall builder (libOps Map.! "OdeShutdown") [] ""
    -- return void
    r <- liftIO $ buildRetVoid builder
    liftIO $ disposeBuilder builder
    return curFunc

  where
    outDataSize = Map.size initVals

    -- attempt at optimised reaction selection by presorting reactions by probability
    simOps' = simOps -- List.sortBy f simOps
--      where
--        f r1 r2 | calcProp r1 == calcProp r2 = Prelude.EQ
--        f r1 r2 | calcProp r1 > calcProp r2 = Prelude.GT
--        f r1 r2 | calcProp r1 < calcProp r2 = Prelude.LT
--
--        calcProp (Rre srcs _ rate) =  (product srcPops) * rate
--          where
--            srcPops = map (\(i, v) -> (initVals Map.! v) * fromIntegral i) srcs


    -- | Create the main body of the SSA solver loop
    createSolverLoopBody  :: LLVM.Value -> LLVM.Value -> LocalMap -> (LLVM.Value, LLVM.Value, LLVM.Value, LLVM.Value) -> GenM ()
    createSolverLoopBody  sumPropRef nextOutputRef stateRefMap (curPeriodRef, curLoopRef, curTimeRef, outStateArray) = do
        GenState {builder, curFunc, simParams} <- get
        tau <- chooseTimestep sumPropRef

        -- choose and trigger reaction
        chooseTriggerReaction sumPropRef stateRefMap

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
                        curLoop'' <- buildUIToFP builder curLoop' doubleType "fpcast"
                        buildFMul builder curLoop'' (constDouble $ L.get Sys.lOutputPeriod simParams) "nextOutput"
                    return curLoop'
                -- write data
                writeOutData outStateArray curTimeRef $ Map.elems stateRefMap
                liftIO $ buildNoOp builder)
            -- ifFalse
            (\builder -> liftIO $ buildNoOp builder)

        -- run the loop exprs (for dyn rate values)
        evalExprs curTimeRef stateRefMap
        -- update sumprops
        sumPropensities sumPropRef stateRefMap

        return ()

    chooseTimestep :: LLVM.Value -> GenM (LLVM.Value)
    chooseTimestep sumPropRef = do
        GenState {builder, libOps, mathOps} <- get
        liftIO $ do
            ts1 <- buildCall builder (libOps Map.! "OdeRandUniform") [] "ts1"
            ts2 <- buildCall builder (mathOps Map.! AC.Log) [ts1] "ts2"
            ts3 <- withPtrVal builder sumPropRef $ \sumProp ->
                buildFDiv builder constOne sumProp "ts3"
            ts4 <- buildFNeg builder ts3 "ts4"
            buildFMul builder ts4 ts2 "tau"

    -- | we choose and trigger the reaction in a single block of code as is easier, already have access to the params within the bb
    chooseTriggerReaction :: LLVM.Value -> LocalMap -> GenM ()
    chooseTriggerReaction sumPropRef stateValsRefMap = do
        GenState {builder, libOps, curFunc} <- get

        r2 <- liftIO $ buildCall builder (libOps Map.! "OdeRandUniform") [] "trRand"
        endProp <- liftIO $ withPtrVal builder sumPropRef $ \sumProp ->
            buildFMul builder r2 sumProp "endProp"

        endBB <- liftIO $ appendBasicBlock curFunc "end.TR"
        startBB <- liftIO $ appendBasicBlock curFunc "start.TR"
        liftIO $ buildBr builder startBB
        checkTrigger builder curFunc endProp endBB constZero startBB simOps' -- gen code to both check if reaction is chosen and to handle trigger
        liftIO $ positionAtEnd builder endBB
        return ()
      where
        -- manual fold over rres - as we need to ensure trigger occurs on last elem, plus codegen is a bit neater & optmised
        checkTrigger builder curFunc endProp endBB curProp curBB (rreOp@(CF.Rre srcs dests rate):simOps) = do
            liftIO $ positionAtEnd builder curBB
            reactionProp <- calcPropensity builder stateValsRefMap rreOp
            curProp' <- liftIO $ buildFAdd builder reactionProp curProp "sumProp"
            triggerBB <- liftIO $ appendBasicBlock curFunc "trigger.TR"
            -- if we're on lastOp - reaction must be triggered
            nextBB <-   liftIO $ if lastOp then do
                            -- jump stragiht to trigger then endBB
                            buildBr builder triggerBB
                            return endBB
                         else do
                            -- build branch to check if this reaction is triggered
                            cmpProp <- buildFCmp builder FPOGT curProp' endProp "cmpProps"
                            nextBB <- appendBasicBlock curFunc "next.TR"
                            buildCondBr builder cmpProp triggerBB nextBB
                            return nextBB

            -- build triggerbb - actually trigger the reaction and update populations
            liftIO $ positionAtEnd builder triggerBB
            mapM_ decPop srcs
            mapM_ incPop dests
            liftIO $ buildBr builder endBB -- we're done, jump to the endBB

            if lastOp   then return ()
                        else checkTrigger builder curFunc endProp endBB curProp' nextBB simOps
              where
                decPop (_, vId) = liftIO $ do
                    updatePtrVal builder (stateValsRefMap Map.! vId) $ \v ->
                        buildFSub builder v (constDouble 1) "decPop"
                incPop (i, vId) = liftIO $ do
                    updatePtrVal builder (stateValsRefMap Map.! vId) $ \v ->
                        buildFAdd builder v (constDouble . fromIntegral $ i) "incPop"
                lastOp = null simOps

        -- non-rre sim op - ignore
        checkTrigger builder curFunc endProp endBB curProp nextBB (op:simOps) = checkTrigger builder curFunc endProp endBB curProp nextBB simOps

    -- | Iterate over the RREs and both calculate the props and sum them up
    sumPropensities :: LLVM.Value -> LocalMap -> GenM (LLVM.Value)
    sumPropensities sumPropRef stateValsRefMap = do
        GenState {builder, simParams, llvmMod} <- get
        (Just sumProp) <- DF.foldlM (sumPropensities' builder) Nothing simOps'
        liftIO $ buildStore builder sumProp sumPropRef
      where
        sumPropensities' builder mCurSum simOp = do
            reactionProp <- calcPropensity builder stateValsRefMap simOp
            case mCurSum of
                Just curSum -> liftIO $ Just <$> buildFAdd builder reactionProp curSum "sumProp"
                Nothing  -> return $ Just reactionProp

    -- | calculate the propensity of a given reaction - only consider elementary reactions
    calcPropensity :: Builder -> LocalMap -> SimOp -> GenM (LLVM.Value)
    calcPropensity builder stateValsRefMap (CF.Rre ((_, src1Id):[]) _ (VarRef rateId)) = do
        src1Pop <- liftIO $ buildLoad builder (stateValsRefMap Map.! src1Id) "src1Pop"
        rateVal <- lookupId rateId
        liftIO $ buildFMul builder src1Pop rateVal "prop"

    calcPropensity builder stateValsRefMap (CF.Rre ((_, src1Id):(_, src2Id):[]) _ (VarRef rateId)) = do
        src1Pop <- liftIO $ buildLoad builder (stateValsRefMap Map.! src1Id) "src1Pop"
        src2Pop <- liftIO $ buildLoad builder (stateValsRefMap Map.! src2Id) "src2Pop"
        prop1 <- liftIO $ buildFMul builder src1Pop src2Pop "prop1"
        rateVal <- lookupId rateId
        liftIO $ buildFMul builder prop1 rateVal "prop2"

    evalExprs :: LLVM.Value -> LocalMap -> GenM ()
    evalExprs curTimeRef stateRefMap = do
        GenState {builder} <- get
        stateValMap <- loadRefMap stateRefMap
        withPtrVal builder curTimeRef $ \curTimeVal -> do
            genExprEval loopExprs curTimeVal stateValMap


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
-- TODO - update to use gatherArray
writeOutData :: LLVM.Value -> LLVM.Value -> [LLVM.Value] -> GenM ()
writeOutData outStateArray curTimeRef stateRefs = do
    GenState {builder, libOps} <- get
    -- fill the state array
    liftIO $ gatherArray builder outStateArray stateRefs
    -- write to output func
    simStateArrayPtr <- liftIO $ buildInBoundsGEP builder outStateArray [constInt64 0, constInt64 0] $ "storeOutPtr"
    liftIO $ withPtrVal builder curTimeRef $ \curTimeVal ->
        buildCall builder (libOps Map.! "OdeWriteState") [curTimeVal, simStateArrayPtr] ""
    -- liftIO $ setInstructionCallConv callInst Fast
    return ()
