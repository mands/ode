-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Simulation.JITCompiler.JITHybridSolver
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Tempory creation of hybrid solver - implements a std. Euler solver with an SSA solver
-- bit of a hack copy-and-paste job atm, solvers all need refactoring into common, reusable components
--
-----------------------------------------------------------------------------

module Subsystem.Simulation.JITCompiler.JITHybridSolver (
genHybridSolver
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
import Subsystem.Simulation.JITCompiler.JITSolver(createSimParams, writeOutData)


genHybridSolver :: CF.Module -> GenM LLVM.Value
genHybridSolver odeMod@CF.Module{..} = do
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

    -- assume a SimHybrid solver, hence pick up the Euler
    solver <- MkSolver <$> (genVals (Map.keys initVals) simOps :: GenM EulerSolver)

    -- create mutable sim params (static sim params embeedded as constants)
    simParamVs@(curPeriodRef, curLoopRef, curTimeRef, outStateArray) <- createSimParams outDataSize

    -- setup and write initial data
    genModelInitials initVals (getStateVals solver)
    writeOutData outStateArray curTimeRef $ Map.elems (getStateVals solver)

    -- SSA - eval expressions and setup init sumprops, use global time
    ssaVs@(sumPropRef, ssaTimeRef) <- initSSA curTimeRef (getStateVals solver)

    -- create the main solver loop
    doWhileStmt builder curFunc
        -- doBody
        (createSolverLoopBody solver odeMod simParamVs ssaVs)
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
    createSolverLoopBody  :: (OdeSolver a) => a -> CF.Module -> (LLVM.Value, LLVM.Value, LLVM.Value, LLVM.Value) ->
        (LLVM.Value, LLVM.Value) -> Builder -> BasicBlock -> GenM LLVM.Value
    createSolverLoopBody solver odeMod simParamVs@(curPeriodRef, curLoopRef, curTimeRef, outStateArray) ssaVs@(sumPropRef, ssaTimeRef) builder breakBB = do
        GenState {curFunc, simParams, libOps} <- get
        -- get the state vals
        let stateRefMap = getStateVals solver
        -- SSA - store cur time in ssaTimeRef
        liftIO $ withPtrVal builder curTimeRef (\curTime -> buildStore builder curTime ssaTimeRef)

        -- inc loop counter & calc the time
        liftIO $ updatePtrVal builder curLoopRef (\curLoop -> buildAdd builder curLoop (constInt64 1) "incCurLoop")
        liftIO $ withPtrVal builder curLoopRef $ \ curLoop -> do
            curLoop' <- buildUIToFP builder curLoop doubleType "convDouble"
            timeDelta <- buildFMul builder curLoop' (constDouble $ L.get Sys.lTimestep simParams) "timeDelta"
            curTime' <- buildFAdd builder timeDelta (constDouble $ L.get Sys.lStartTime simParams) "curTime"
            buildStore builder curTime' curTimeRef

        -- SSA - run the SSA upto dt
        runSSA odeMod stateRefMap curTimeRef ssaVs

        -- call the fixed-dt ODE solver
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
                buildNoOp builder)

        liftIO $ withPtrVal builder curTimeRef $ \curTime ->
            debugStmt builder libOps "Ran Ode - time : %g\n" [curTime]
        -- return noop
        buildNoOp builder


    -- SSA Code --------------------------------------------------------------------------------------------------------

    rreSimOps = filter (\op -> case op of (CF.Rre _ _ _) -> True; _ -> False) simOps

    -- alloc vars needed for SSA loop
    initSSA :: LLVM.Value -> LocalMap -> GenM (LLVM.Value, LLVM.Value)
    initSSA curTimeRef stateRefMap = do
        GenState {builder} <- get
        ssaTimeRef <- liftIO $ buildAlloca builder doubleType "ssaTime"
        sumPropRef <- liftIO $ buildAlloca builder doubleType "sumProp"
        return (sumPropRef, ssaTimeRef)


    -- SSA - runs an SSA from t -> t + dt, for all RRE simOps - this code taken direct from SSA solver, needs to be integrated properly
    runSSA :: CF.Module -> LocalMap -> LLVM.Value -> (LLVM.Value, LLVM.Value) -> GenM ()
    runSSA odeMod stateRefMap curTimeRef (sumPropRef, ssaTimeRef) = do
        GenState {builder, curFunc} <- get
        -- SSA - loop converted to run until dt only, not adjustedStopTime

        -- create the main solver loop - use while stmt
        whileStmt builder curFunc
            -- whileCond, ssaTime < curTime
            (\builder ->
                liftIO $ withPtrVal builder ssaTimeRef $ \ssaTime ->
                liftIO $ withPtrVal builder curTimeRef $ \curTime ->
                    buildFCmp builder FPOLT ssaTime curTime "bWhileTime")
            -- whileBody
            (createSSALoopBody)

      where
        -- | Create the main body of the SSA solver loop
        createSSALoopBody  :: Builder -> BasicBlock -> GenM LLVM.Value
        createSSALoopBody builder breakBB = do
            GenState {curFunc, simParams, libOps} <- get

            -- SSA - calc sum Props using new SSATime
            -- run the loop exprs (for dyn rate values)
            evalExprs ssaTimeRef stateRefMap
            -- update sumprops
            sumPropensities sumPropRef stateRefMap

            -- SSA - cont only if sumProps >= 0
            contStmt builder curFunc breakBB $ \builder -> do
                liftIO $ withPtrVal builder sumPropRef $ \sumProp -> buildFCmp builder FPOGT sumProp constZero "propCheck"

            -- choose timestep (constly, so break earlier when poss)
            tau <- chooseTimestep sumPropRef

            -- SSA - cont if tau + ssaTime <= curTime
            contStmt builder curFunc breakBB $ \builder -> do
                liftIO $    withPtrVal builder ssaTimeRef $ \ssaTime ->
                            withPtrVal builder curTimeRef $ \curTime -> do
                                ssaTime' <- buildFAdd builder ssaTime tau "nextSSATime"
                                debugStmt builder libOps "In SSA - curTime : %.6f, ssaTime : %.6f, tau : %.6f, nextSSATime - %.6f\n" [curTime, ssaTime, tau, ssaTime']
                                buildFCmp builder FPOLE ssaTime' curTime "tauCheck"

            -- choose and trigger reaction
            chooseTriggerReaction sumPropRef stateRefMap

            -- update time
            liftIO $ updatePtrVal builder ssaTimeRef $ \ssaTime ->
                buildFAdd builder ssaTime tau "incTime"

            liftIO $    withPtrVal builder sumPropRef $ \sumProp ->
                        withPtrVal builder ssaTimeRef $ \ssaTime ->
                            debugStmt builder libOps "Ran SSA - sumProp : %g, new SSA time : %g\n" [sumProp, ssaTime]
            -- return a noop
            buildNoOp builder

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

        liftIO $ debugStmt builder libOps "In SSA - endProp : %g\n" [endProp]


        endBB <- liftIO $ appendBasicBlock curFunc "end.TR"
        startBB <- liftIO $ appendBasicBlock curFunc "start.TR"
        liftIO $ buildBr builder startBB
        checkTrigger builder curFunc endProp endBB constZero startBB rreSimOps -- gen code to both check if reaction is chosen and to handle trigger
        positionAtEnd' builder endBB
        return ()
      where
        -- manual fold over rres - as we need to ensure trigger occurs on last elem, plus codegen is a bit neater & optmised
        checkTrigger builder curFunc endProp endBB curProp curBB (rreOp@(CF.Rre srcs dests rate):simOps) = do
            GenState {libOps} <- get
            positionAtEnd' builder curBB
            reactionProp <- calcPropensity builder stateValsRefMap rreOp
            curProp' <- liftIO $ buildFAdd builder reactionProp curProp "accProp"

            liftIO $ debugStmt builder libOps "In SSA - accProp : %g\n" [curProp']

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
            positionAtEnd' builder triggerBB
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
        (Just sumProp) <- DF.foldlM (sumPropensities' builder) Nothing rreSimOps
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

    -- eval the expressions in a new env (needed for rate exprs)
    evalExprs :: LLVM.Value -> LocalMap -> GenM ()
    evalExprs curTimeRef stateRefMap = do
        GenState {builder} <- get
        stateValMap <- loadRefMap stateRefMap
        withPtrVal builder curTimeRef $ \curTimeVal -> do
            genExprEval loopExprs curTimeVal stateValMap

