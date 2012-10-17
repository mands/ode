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

module Subsystem.Simulation.JITCompiler.JITMain (
runSimulation, createJITModule
) where


-- Labels
import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)

-- LLVM code
import LLVM.Wrapper.Core as LLVM
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
    -- insert the math ops
    extOps <- liftIO $ defineMathOps llvmMod
    modify (\st -> st { llvmMod, extOps })

    -- insert global vals - sim vals, states and deltas
    gSimTime <- liftIO $ addGlobal llvmMod (doubleType) "_simTime"
    liftIO $ LFFI.setInitializer gSimTime $ constReal doubleType (FFI.CDouble 0.03)


    -- insert the funcs into the module
    initsF <-   genModelInitials odeMod
    loopF <-    genModelLoop odeMod
    simF <-     genModelSolver odeMod initsF loopF

    -- we hand-generate the solver here - could
    -- _ <- genModelSolver mMod

    -- defineModule mMod $ genModelInitials mod
    -- defineModule mMod $ genModelDeltas mod
    -- save the module to disk
    liftIO $ printModuleToFile llvmMod "model.ll"
    liftIO $ writeBitcodeToFile llvmMod "model.bc"
    -- run our external script - this runs our optimisations and links to the Ode run-time library

    -- return the update module
    return ()


-- Code Generation -----------------------------------------------------------------------------------------------------
-- We only codegen the initial val and delta fucntion calculation, other funcs provided within the std. library for now
-- this includes init/startup and shutdown funcs, and the solvers (for now a forward Euler and RK4)

genModelInitials :: CF.Module -> GenM LLVM.Value
genModelInitials CF.Module{..} = do
    -- define the func
    GenState {llvmMod} <- get
    curFunc <- liftIO $ addFunction llvmMod "modelInitials" (functionType voidType createArgsList False)
    builder <- liftIO $ createBuilder
    -- create the entry block & pos the builder
    entryBB <- liftIO $ appendBasicBlock curFunc "entry"
    liftIO $ positionAtEnd builder entryBB
    -- store the builder
    modify (\st -> st { builder, curFunc })
    -- add the insts (if exprMap not empty)
    unless (OrdMap.null initExprs) (void $ genExprMap initExprs)
    -- store the outputs
    storeOutputs
    -- return void
    r <- liftIO $ buildRetVoid builder
    liftIO $ disposeBuilder builder
    return curFunc
  where
    -- create the input args
    createArgsList = map (\(_, (ExprData _ t)) -> pointerType (convertType t) 0)
       (OrdMap.toList $ initExprs)

    -- setup the store commands for the outputs
    storeOutputs = do
        GenState {builder, curFunc} <- get
        params <- liftIO $ LLVM.getParams curFunc
        -- zip the ids (from toplets) with the params (the ordering will be the same)
        let outVals = zip (map fst (OrdMap.toList initExprs)) params
        -- for each val, gen the store thru the pointer
        forM_ outVals $ \(i, outVal) -> do
            initVal <- lookupId i
            liftIO $ buildStore builder initVal outVal

genModelLoop :: CF.Module -> GenM LLVM.Value
genModelLoop CF.Module{..} = do
    -- define the func
    GenState {llvmMod} <- get
    curFunc <- liftIO $ addFunction llvmMod "modelLoop" (functionType voidType createArgsList False)
    createLocalMap curFunc
    -- create the entry block & pos the builder
    entryBB <- liftIO $ appendBasicBlock curFunc "entry"
    builder <- liftIO $ createBuilder
    liftIO $ positionAtEnd builder entryBB
    -- store the builder
    modify (\st -> st { builder, curFunc })
    -- add the insts (if exprMap not empty)
    unless (OrdMap.null loopExprs) (void $ genExprMap loopExprs)
    -- store the outputs
    storeOutputs
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
    storeOutputs = do
        GenState {builder, curFunc} <- get
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

genModelSolver :: CF.Module -> LLVM.Value -> LLVM.Value -> GenM ()
genModelSolver CF.Module{..} initsF loopF = do
    undefined






