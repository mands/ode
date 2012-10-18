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

module Subsystem.Simulation.JITCompiler.JITMain (
runSimulation, createJITModule
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
import Subsystem.Simulation.JITCompiler.JITCoreFlat


-- JIT Setup -----------------------------------------------------------------------------------------------------------


runSimulation :: IO ()
runSimulation = return ()

createJITModule :: CF.Module -> GenM ()
createJITModule odeMod = do
    -- gen the JIT funcs

    -- create the module to disk
    llvmMod <- liftIO $ moduleCreateWithName "model"
    -- insert the math ops and lib ops
    (mathOps, libOps) <- liftIO $ defineExtOps llvmMod
    modify (\st -> st { llvmMod, mathOps, libOps })

    -- insert global vals - sim vals, states and deltas
    gSimTime <- liftIO $ addGlobal llvmMod (doubleType) "_simTime"
    liftIO $ LFFI.setInitializer gSimTime $ constReal doubleType (FFI.CDouble 0.03)

    -- generate & insert the funcs into the module
    initsF <-   genModelInitials odeMod
    loopF <-    genModelLoop odeMod
    simF <-     genModelSolver odeMod initsF loopF

    -- save the module to disk
    liftIO $ printModuleToFile llvmMod "model.ll"
    liftIO $ writeBitcodeToFile llvmMod "model.bc"
    -- run our external script - this runs our optimisations and links to the Ode run-time library

    -- return the update module
    return ()


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
    (curFunc, builder) <- genFunction  "modelInitials" voidType createArgsList
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
    createArgsList = map (\(_, (ExprData _ t)) -> pointerType (convertType t) 0)
       (OrdMap.toList $ initExprs)

    -- setup the store commands for the outputs
    storeOutputs curFunc builder = do
        params <- liftIO $ LLVM.getParams curFunc
        -- zip the ids (from toplets) with the params (the ordering will be the same)
        let outVals = zip (map fst (OrdMap.toList initExprs)) params
        -- for each val, gen the store thru the pointer
        forM_ outVals $ \(i, outVal) -> do
            initVal <- lookupId i
            liftIO $ buildStore builder initVal outVal

genModelLoop :: CF.Module -> GenM LLVM.Value
genModelLoop CF.Module{..} = do
    (curFunc, builder) <- genFunction  "modelLoop" voidType createArgsList
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
    createArgsList =    (OrdMap.elems . fmap (\(ExprData _ t) -> convertType t) $ initExprs) ++
                        (OrdMap.elems . fmap (\(ExprData _ t) -> pointerType (convertType t) 0) $ initExprs)

    createLocalMap curFunc = do
        params <- liftIO $ LLVM.getParams curFunc
        let inParams = take (length params `div` 2) params
        let localMap = Map.fromList $ zip (OrdMap.keys initExprs) inParams
        modify (\st -> st { localMap })

    -- need map over simops
    storeOutputs curFunc builder = do
        params <- liftIO $ LLVM.getParams curFunc
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
    GenState {libOps, llvmMod, simParams} <- get

    -- call the start_sim func
    _ <- liftIO $ buildCall builder (libOps Map.! "init") [] ""
    fileStr <- liftIO $ createConstString llvmMod (L.get Sys.lFilename simParams)
    fileStrPtr <- liftIO $ buildInBoundsGEP builder fileStr [constInt32' 0, constInt32' 0] ""
    _ <- liftIO $ buildCall builder (libOps Map.! "start_sim") [fileStrPtr] ""

    -- create the vals
    stateValMap <- createVals (OrdMap.keys initExprs) False
    deltaValMap <- createVals (OrdMap.keys initExprs) True

    -- create variable sim params (static sim params embeedded as constants)
    simParamVs@(simCurPeriod, simCurLoop, simCurTime, simOutData) <- createSimParams
    writeOutData simOutData $ OrdMap.elems stateValMap

    -- call the init funcs
    _ <- liftIO $ buildCall builder initsF (OrdMap.elems stateValMap) ""

    -- create the main solver loop
    doWhileStmt builder curFunc
        (\builder ->  createSolverLoopBody stateValMap deltaValMap simParamVs >> (liftIO $ buildNoOp builder))
        (\builder _ ->  do
            liftIO $ withPtrVal builder simCurTime $ \curTime -> do
                liftIO $ buildFCmp builder FPOLT curTime (constDouble $ L.get Sys.lEndTime simParams) "bWhileTime")

    -- end sim
    _ <- liftIO $ buildCall builder (libOps Map.! "end_sim") [] ""
    _ <- liftIO $ buildCall builder (libOps Map.! "shutdown") [] ""

    -- return void
    r <- liftIO $ buildRetVoid builder
    liftIO $ disposeBuilder builder
    return curFunc
  where
    -- create the main state and delta variables
    createVals :: [Id] -> Bool -> GenM (OrdMap.OrdMap Id LLVM.Value)
    createVals ids isDelta = foldM createVal OrdMap.empty ids
      where
        createVal idMap i = do
            GenState {builder} <- get
            llV <- liftIO $ buildAlloca builder doubleType (getName i)
            return $ OrdMap.insert i llV idMap
        getName i = if isDelta then (getValidIdName i) ++ "delta" else (getValidIdName i)

    -- create most (mutable) sim params
    createSimParams :: GenM (LLVM.Value, LLVM.Value, LLVM.Value, LLVM.Value)
    createSimParams = do
        GenState {builder, simParams} <- get
        simCurPeriod <- liftIO $ buildAllocaWithInit builder (constInt' 1) int64Type "simCurPeriod"
        simCurLoop <- liftIO $ buildAllocaWithInit builder (constInt' 0) int64Type "simCurLoop"

        -- set inital time
        simCurTime <- liftIO $ buildAllocaWithInit builder (constDouble $ L.get Sys.lStartTime simParams)
            doubleType "simCurTime"

        -- set output vector
        simOutData <- liftIO $ buildAlloca builder (LFFI.arrayType doubleType (fromIntegral $ OrdMap.size initExprs)) "simOutData"

        return (simCurPeriod, simCurLoop, simCurTime, simOutData)

    writeOutData :: LLVM.Value -> [LLVM.Value] -> GenM ()
    writeOutData simOutData stateVals = do
        GenState {builder, libOps} <- get
        forM_ (zip [0..] stateVals) $ \(i, stateVal) -> do
            loadV <- liftIO $ buildLoad builder stateVal $ "loadState" ++ (show i)
            outV <- liftIO $ buildInBoundsGEP builder simOutData [constInt32' 0, constInt32' i] $ "storeOutPtr" ++ (show i)
            _ <- liftIO $ buildStore builder loadV outV
            return ()
        -- write to output func
        simOutDataPtr <- liftIO $ buildInBoundsGEP builder simOutData [constInt32' 0, constInt32' 0] $ "storeOutPtr"
        _ <- liftIO $ buildCall builder (libOps Map.! "write_dbls_arr") [constInt32' $ OrdMap.size initExprs, simOutDataPtr] ""
        return ()

    createSolverLoopBody  :: ParamMap -> ParamMap -> (LLVM.Value, LLVM.Value, LLVM.Value, LLVM.Value) -> GenM ()
    createSolverLoopBody  stateValMap deltaValMap (simCurPeriod, simCurLoop, simCurTime, simOutData) = do
        GenState {builder, curFunc, simParams} <- get

        -- inc loop counter & calc the time
        liftIO $ updatePtrVal builder simCurLoop (\curLoop -> buildAdd builder curLoop (constInt' 1) "incCurLoop")
        liftIO $ withPtrVal builder simCurLoop $ \ curLoop -> do
            curLoop' <- buildUIToFP builder curLoop doubleType "convDouble"
            timeDelta <- buildFMul builder curLoop' (constDouble $ L.get Sys.lTimestep simParams) "timeDelta"
            curTime <- buildFAdd builder timeDelta (constDouble $ L.get Sys.lStartTime simParams) "curTime"
            buildStore builder curTime simCurTime

        -- call the modelLoop func
        stateVals <- mapM (\v -> liftIO $ buildLoad builder v "") $ OrdMap.elems stateValMap
        _ <- liftIO $ buildCall builder loopF (stateVals  ++ OrdMap.elems deltaValMap) ""

        -- update the states/run the forward euler
        liftIO $ mapM_ (updateState builder simParams) simOps

        bWriteOut <- liftIO $ withPtrVal builder simCurPeriod $ \curPeriod -> do
            buildICmp builder IntEQ curPeriod (constInt' $ L.get Sys.lOutputPeriod simParams) "bWriteOut"

        -- check period and writeOutData if needed
        _ <- ifStmt builder curFunc bWriteOut
            (\builder -> do
                _ <- writeOutData simOutData $ OrdMap.elems stateValMap
                liftIO $ buildStore builder (constInt' 1) simCurPeriod)
            (\builder -> do
                liftIO $ updatePtrVal builder simCurPeriod $ \curPeriod -> buildAdd builder curPeriod (constInt' 1) "incCurPeriod"
                liftIO $ buildNoOp builder)

        return ()
      where
        updateState :: Builder -> Sys.SimParams -> SimOps -> IO ()
        updateState builder simParams (Ode i dV) = do
            -- get state val
            updatePtrVal builder (stateValMap OrdMap.! i) $ \stateVal -> do
                withPtrVal builder (deltaValMap OrdMap.! i) $ \dVal -> do
                    dValTime <- buildFMul builder dVal (constDouble $ L.get Sys.lTimestep simParams) "deltaTime"
                    buildFAdd builder stateVal dValTime "newState"



