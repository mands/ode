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
genModelInitials, genModelLoop, genModelSolver, genAOTMain
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


-- JIT Setup -----------------------------------------------------------------------------------------------------------

-- Code Generation -----------------------------------------------------------------------------------------------------
-- We only codegen the initial val and delta fucntion calculation, other funcs provided within the std. library for now
-- this includes init/startup and shutdown funcs, and the solvers (for now a forward Euler and RK4)

-- | A helper function that performs much of the boiler-plate code in creating a function
genFunction :: String -> LLVM.Type -> [LLVM.Type] -> GenM (LLVM.Value, LLVM.Builder)
genFunction fName fRetType fArgTypes = do
    -- define the func
    GenState {llvmMod} <- get
    curFunc <- liftIO $ addFunction llvmMod fName (functionType fRetType fArgTypes False)
    builder <- liftIO $ createBuilder
    -- create the entry block & pos the builder
    entryBB <- liftIO $ appendBasicBlock curFunc "entry"
    liftIO $ positionAtEnd builder entryBB
    -- store and return the func & builder
    modify (\st -> st { builder, curFunc, localMap = Map.empty })
    return (curFunc, builder)


genModelInitials :: CF.Module -> GenM LLVM.Value
genModelInitials CF.Module{..} = do
    (curFunc, builder) <- genFunction "modelInitials" voidType createArgsList
    liftIO $ setLinkage curFunc PrivateLinkage
    -- set func params
    _ <- liftIO $ addFuncAttributes curFunc [AlwaysInlineAttribute, NoUnwindAttribute]
    liftIO $ getParams curFunc >>= \params -> setParamAttribs params

    -- add the insts (if exprMap not empty)
    unless (OrdMap.null initExprs) (void $ genExprMap initExprs)
    -- store the outputs
    storeOutputs curFunc builder
    -- return void
    r <- liftIO $ buildRetVoid builder
    liftIO $ disposeBuilder builder
    return curFunc
  where
    -- create the input args
    createArgsList = doubleType : (OrdMap.elems . fmap (\(ExprData _ t) -> (pointerType (convertType t) 0)) $ initExprs)

    setParamAttribs res@(t:outParams) = do
        forM_ outParams $ \param -> addParamAttributes param [NoAliasAttribute, NoCaptureAttribute]
        return res

    -- setup the store commands for the outputs
    storeOutputs curFunc builder = do
        (curTimeVal : params) <- liftIO $ LLVM.getParams curFunc
        modify (\st -> st { curTimeVal })
        -- zip the ids (from toplets) with the params (the ordering will be the same)
        let outVals = zip (map fst (OrdMap.toList initExprs)) params
        -- for each val, gen the store thru the pointer
        forM_ outVals $ \(i, outVal) -> do
            initVal <- lookupId i
            liftIO $ buildStore builder initVal outVal

genModelLoop :: CF.Module -> GenM LLVM.Value
genModelLoop CF.Module{..} = do
    (curFunc, builder) <- genFunction  "modelLoop" voidType createArgsList
    liftIO $ setLinkage curFunc PrivateLinkage
    -- set func params
    _ <- liftIO $ addFuncAttributes curFunc [AlwaysInlineAttribute, NoUnwindAttribute]
    liftIO $ getParams curFunc >>= \params -> setParamAttribs params
    -- setup access to the input args
    createLocalMap curFunc
    -- add the insts (if exprMap not empty)
    unless (OrdMap.null loopExprs) (void $ genExprMap loopExprs)
    -- store the outputs
    storeOutputs curFunc builder
    -- return void
    r <- liftIO $ buildRetVoid builder
    liftIO $ disposeBuilder builder
    return curFunc
  where
    createArgsList =    doubleType : (OrdMap.elems . fmap (\(ExprData _ t) -> convertType t) $ initExprs) ++
                        (OrdMap.elems . fmap (\(ExprData _ t) -> pointerType (convertType t) 0) $ initExprs)

    setParamAttribs res@(t:params) = do
        let outParams = drop (length params `div` 2) params
        forM_ outParams $ \param -> addParamAttributes param [NoAliasAttribute, NoCaptureAttribute]
        return res

    createLocalMap curFunc = do
        (curTimeVal : params) <- liftIO $ LLVM.getParams curFunc
        modify (\st -> st { curTimeVal })
        let inParams = take (length params `div` 2) params
        let localMap = Map.fromList $ zip (OrdMap.keys initExprs) inParams
        modify (\st -> st { localMap })

    -- need map over simops
    storeOutputs curFunc builder = do
        (_ : params) <- liftIO $ LLVM.getParams curFunc
        let outParams = drop (length params `div` 2) params
        let outMap = Map.fromList $ zip (OrdMap.keys initExprs) outParams
        -- map over simops
        forM_ simOps $ storeDelta outMap builder
      where
        storeDelta outMap builder (Ode initId ((VarRef deltaId))) = do
            deltaVal <- lookupId deltaId
            let outVal = outMap Map.! initId
            liftIO $ buildStore builder deltaVal outVal

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

    -- create the vals
    stateValRefMap <- createVals (OrdMap.keys initExprs) "StateRef"
    deltaValRefMap <- createVals (OrdMap.keys initExprs) "DeltaRef"
    -- create variable sim params (static sim params embeedded as constants)
    simParamVs@(curPeriodRef, curLoopRef, curTimeRef, outDataRef) <- createSimParams

    -- call the init funcs
    _ <- liftIO $ withPtrVal builder curTimeRef $ \curTime -> do
        buildCall builder initsF (curTime : OrdMap.elems stateValRefMap) ""
    -- write initial data
    writeOutData outDataRef curTimeRef $ OrdMap.elems stateValRefMap

    -- create the main solver loop
    doWhileStmt builder curFunc
        -- doBody
        (\builder ->  createSolverLoopBody stateValRefMap deltaValRefMap simParamVs >> (liftIO $ buildNoOp builder))
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
    -- create the main state and delta variables
    createVals :: [Id] -> String -> GenM (OrdMap.OrdMap Id LLVM.Value)
    createVals ids suffix = foldM createVal OrdMap.empty ids
      where
        createVal idMap i = do
            GenState {builder, llvmMod} <- get
            llV <- liftIO $ addGlobalWithInit llvmMod (constDouble 0.0) doubleType (getName i)
            liftIO $ setLinkage llV PrivateLinkage
            return $ OrdMap.insert i llV idMap
        getName i = (getValidIdName i) ++ suffix

    -- create most (mutable) sim params
    createSimParams :: GenM (LLVM.Value, LLVM.Value, LLVM.Value, LLVM.Value)
    createSimParams = do
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
      where
        outDataSize = OrdMap.size initExprs + 1

    writeOutData :: LLVM.Value -> LLVM.Value -> [LLVM.Value] -> GenM ()
    writeOutData simOutData curTimeRef stateValRefs = do
        GenState {builder, libOps} <- get
        -- fill the output array (inc curTime)
        forM_ (zip [0..] (curTimeRef:stateValRefs)) $ \(i, ptrVal) -> do
            outV <- liftIO $ buildInBoundsGEP builder simOutData [constInt64 0, constInt64 i] $ "storeOutPtr" ++ (show i)
            liftIO $ withPtrVal builder ptrVal $ \loadV -> liftIO $ buildStore builder loadV outV
            return ()
        -- write to output func
        simOutDataPtr <- liftIO $ buildInBoundsGEP builder simOutData [constInt64 0, constInt64 0] $ "storeOutPtr"
        callInst <- liftIO $ buildCall builder (libOps Map.! "writeDbls") [simOutDataPtr, constInt64 $ OrdMap.size initExprs + 1] ""
        -- liftIO $ setInstructionCallConv callInst Fast
        return ()

    createSolverLoopBody  :: ParamMap -> ParamMap -> (LLVM.Value, LLVM.Value, LLVM.Value, LLVM.Value) -> GenM ()
    createSolverLoopBody  stateValRefMap deltaValRefMap (curPeriodRef, curLoopRef, curTimeRef, outDataRef) = do
        GenState {builder, curFunc, simParams} <- get

        -- inc loop counter & calc the time
        liftIO $ updatePtrVal builder curLoopRef (\curLoop -> buildAdd builder curLoop (constInt64 1) "incCurLoop")
        liftIO $ withPtrVal builder curLoopRef $ \ curLoop -> do
            curLoop' <- buildUIToFP builder curLoop doubleType "convDouble"
            timeDelta <- buildFMul builder curLoop' (constDouble $ L.get Sys.lTimestep simParams) "timeDelta"
            curTime <- buildFAdd builder timeDelta (constDouble $ L.get Sys.lStartTime simParams) "curTime"
            buildStore builder curTime curTimeRef

        -- call the modelLoop func
        stateVals <- mapM (\v -> liftIO $ buildLoad builder v "odeValx") $ OrdMap.elems stateValRefMap
        _ <- liftIO $ withPtrVal builder curTimeRef $ \curTime -> do
            buildCall builder loopF (curTime : stateVals  ++ OrdMap.elems deltaValRefMap) ""

        -- update the states/run the forward euler
        liftIO $ mapM_ (updateState builder simParams) simOps

        bWriteOut <- liftIO $ withPtrVal builder curPeriodRef $ \curPeriod -> do
            buildICmp builder IntEQ curPeriod (constInt64 $ L.get Sys.lOutputPeriod simParams) "bWriteOut"

        -- check period and writeOutData if needed
        _ <- ifStmt builder curFunc bWriteOut
            -- ifTrue
            (\builder -> do
                _ <- writeOutData outDataRef curTimeRef $ OrdMap.elems stateValRefMap
                liftIO $ buildStore builder (constInt64 1) curPeriodRef)
            -- ifFalse
            (\builder -> do
                liftIO $ updatePtrVal builder curPeriodRef $ \curPeriod -> buildAdd builder curPeriod (constInt64 1) "incCurPeriod"
                liftIO $ buildNoOp builder)

        return ()
      where
        updateState :: Builder -> Sys.SimParams -> SimOps -> IO ()
        updateState builder simParams (Ode i dV) = do
            -- get state val
            updatePtrVal builder (stateValRefMap OrdMap.! i) $ \stateVal -> do
                withPtrVal builder (deltaValRefMap OrdMap.! i) $ \dVal -> do
                    dValTime <- buildFMul builder dVal (constDouble $ L.get Sys.lTimestep simParams) "deltaTime"
                    buildFAdd builder stateVal dValTime "newState"

-- | A stub main function used for AOT compilation
genAOTMain :: LLVM.Value -> GenM ()
genAOTMain simF = do
    (curFunc, builder) <- genFunction "main" int32Type []
    -- call the sim func
    _ <- liftIO $ buildCall builder simF [] ""
    liftIO $ buildRet builder $ constInt32 0
    return ()
