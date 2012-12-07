-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Simulation.JITCompiler.JITCoreFlat
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Low level functions that actually handle generating valid LLVM code from the CoreFlat module representation
-- Much of this code is a similar to the interpreter code, generating LLVM values rather than actualling simulating the model
-----------------------------------------------------------------------------

module Subsystem.Simulation.JITCompiler.JITModel (
genModelInitials, genModelRHS,
genExprMap, genExpr, genVar
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

-- Main Model interface ------------------------------------------------------------------------------------------------
-- We only codegen the initial val and delta fucntion calculation, other funcs provided within the std. library for now

-- | Generate the function that creates the inital calues for the model into the STATE variables
-- odeModelInitials(time) -> STATE
genModelInitials :: CF.Module -> GenM LLVM.Value
genModelInitials CF.Module{..} = do
    (curFunc, builder) <- genFunction "modelInitials" voidType createArgsList
    liftIO $ setLinkage curFunc PrivateLinkage
    -- set func params
    _ <- liftIO $ addFuncAttributes curFunc [AlwaysInlineAttribute, NoUnwindAttribute]
    liftIO $ getParams curFunc >>= \params -> setParamAttribs params
    -- store the (const) outputs directly
    storeOutputs curFunc builder
    -- return void
    r <- liftIO $ buildRetVoid builder
    liftIO $ disposeBuilder builder
    return curFunc
  where
    -- create the input args - TODO - why do we include all types here?
    createArgsList = doubleType : (replicate (Map.size initVals) (pointerType doubleType 0))

    setParamAttribs res@(t:outParams) = do
        forM_ outParams $ \param -> addParamAttributes param [NoAliasAttribute, NoCaptureAttribute]
        return res

    -- setup the store commands for the outputs
    storeOutputs curFunc builder = do
        (curTimeVal : params) <- liftIO $ LLVM.getParams curFunc
        modify (\st -> st { curTimeVal })
        -- zip the ids (from toplets) with the params (the ordering will be the same)
        let outVals = zip (Map.keys initVals) params
        -- for each val, gen the store thru the pointer
        forM_ outVals $ \(i, outVal) -> do
            let initVal = initVals Map.! i
            liftIO $ buildStore builder (constDouble initVal) outVal

-- | Generate the function that calculates the DELTA variables based on the current time and STATE, handles both Odes and Sdes
-- odeModelRHS(time, STATE) -> (DELTA, WEINER), i.e. (y', w') = f(t, y)
genModelRHS :: CF.Module -> GenM LLVM.Value
genModelRHS CF.Module{..} = do
    (curFunc, builder) <- genFunction "modelRHS" voidType createArgsList
    liftIO $ setLinkage curFunc PrivateLinkage
    _ <- liftIO $ addFuncAttributes curFunc [AlwaysInlineAttribute, NoUnwindAttribute]
    -- set func params attribs
    (curTimeVal, inParams, dOutParams, wOutParams) <- getFuncParams curFunc
    liftIO $ forM_ (dOutParams ++ wOutParams) $ \param -> addParamAttributes param [NoAliasAttribute, NoCaptureAttribute]
    -- setup access to the input args
    createLocalMap curTimeVal inParams
    -- add the insts (if exprMap not empty)
    unless (OrdMap.null loopExprs) (void $ genExprMap loopExprs)
    -- store the outputs
    storeOutputs builder dOutParams wOutParams
    -- return void
    r <- liftIO $ buildRetVoid builder
    liftIO $ disposeBuilder builder
    return curFunc
  where
    isSde = simType == CF.SimSDE

    createArgsList = doubleType : (replicate (Map.size initVals) doubleType)
                                ++ (replicate (Map.size initVals) (pointerType doubleType 0))
                                ++ if isSde then (replicate (Map.size initVals) (pointerType doubleType 0)) else []

    getFuncParams curFunc = do
        (curTimeVal : params) <- liftIO $ LLVM.getParams curFunc
        let paramSize = (length params) `div` (if isSde then 3 else 2)
        let inParams = take paramSize params
        let (dOutParams, wOutParams) = drop paramSize params |> (if isSde then splitAt paramSize else (\ps -> (ps, [])))
        return (curTimeVal, inParams, dOutParams, wOutParams)

    createLocalMap curTimeVal inParams = do
        let localMap = Map.fromList $ zip (Map.keys initVals) inParams
        modify (\st -> st { curTimeVal, localMap })

    -- need map over simops
    storeOutputs :: LLVM.Builder -> [LLVM.Value] -> [LLVM.Value] -> GenM ()
    storeOutputs builder dOutParams wOutParams = do
        let dOutMap = Map.fromList $ zip (Map.keys initVals) dOutParams
        let wOutMap = Map.fromList $ zip (Map.keys initVals) wOutParams
        forM_ simOps $ storeDelta dOutMap wOutMap builder
      where
        storeDelta dOutMap _ builder (Ode initId (VarRef deltaId)) = do
            deltaVal <- lookupId deltaId
            let deltaOutVal = dOutMap Map.! initId
            void $ liftIO $ buildStore builder deltaVal deltaOutVal

        storeDelta dOutMap wOutMap builder (Sde initId (VarRef weinerId) (VarRef deltaId)) = do
            deltaVal <- lookupId deltaId
            let deltaOutVal = dOutMap Map.! initId
            void $ liftIO $ buildStore builder deltaVal deltaOutVal
            weinerVal <- lookupId weinerId
            let weinerOutVal = wOutMap Map.! initId
            void $ liftIO $ buildStore builder weinerVal weinerOutVal

        storeDelta _ _ _ simOp = errorDump [MkSB simOp] "Not supported by this simulation backend" assert

-- General Expr Generation ---------------------------------------------------------------------------------------------

genExprMap :: ExprMap -> GenM LLVM.Value
genExprMap exprMap = do
    _ <- DF.mapM_ genTopLet exprBody
    -- need to return the last val
    retV <- genTopLet exprRet
    return $ retV
  where
    -- split the exprMap into its body and final/return value
    exprBody    = if (OrdMap.size exprMap > 1) then init $ OrdMap.toList exprMap else []
    exprRet     = last $ OrdMap.toList exprMap

    genTopLet :: (Id, ExprData) -> GenM LLVM.Value
    genTopLet (i, eD) = do
        llV <- genExpr i eD
        -- add the value to the scope map
        modify (\st@(GenState {localMap}) -> st {localMap = Map.insert i llV localMap })
        return llV

-- | Generate a single expression
-- we both generate the expr and store it here locally
genExpr :: Id -> ExprData -> GenM (LLVM.Value)
genExpr i (ExprData (Var v) t) = do
    -- need to allocate an expr, depends on the type and value generated
    llV <- genVar v
    -- trace' [MkSB i, MkSB v] "Adding var to localmap" $ return ()
    return llV

-- process vs, then op
genExpr i (ExprData (Op op vs) t) = do
    vs' <- mapM genVar vs
    let buildOpF = genOp op vs'
    -- call the build op func
    GenState { builder, mathOps } <- get
    llV <- liftIO $ buildOpF builder mathOps (getValidIdName i)
    -- trace' [MkSB i, MkSB op] "Created op func" $ return ()
    return llV

-- process the cond, then BBs for each branch and a phi to connect them
genExpr i (ExprData (If vB emT emF) t) = do
    -- gen the if test
    llVB <- genVar vB
    GenState { builder, curFunc } <- get
    phis <- ifStmt builder curFunc llVB (\builder -> genExprMap emT) (\builder -> genExprMap emF)

    -- handle phis from both if branches
    llPhi <- liftIO $ buildPhi builder (convertType t) (getValidIdName i)
    liftIO $ addIncoming llPhi phis
    return llPhi

-- | Gen a var operation
genVar :: Var -> GenM LLVM.Value
-- Complex vars
-- refs lookup in env
genVar (VarRef i) = lookupId i

-- tuples require LLVM structs
-- TODO - should create the builder func with the toplet name
genVar (TupleRef i tupIdx) = do
    -- trace' [MkSB i, MkSB tupIdx] "Lookup in a const tuple" $ return ()
    llTupleV <- lookupId i
    GenState {builder} <- get
    liftIO $ buildExtractValue builder llTupleV (fromIntegral $ tupIdx - 1) ""

-- simple map over the vars and dynamically build a struct from the vals
genVar (Tuple vs) = do
    -- trace' [MkSB vs] "Building a const tuple" $ return ()
    llVs <- mapM genVar vs
    -- get the llvm types and create a struct
    types <- liftIO $ mapM LFFI.typeOf llVs
    let sType = structType types False
    -- trace' [MkSB llVs, MkSB types, MkSB sType] "tuple vals" $ return ()
    GenState {builder} <- get
    -- get a undef and populate with vals
    liftIO $ DF.foldlM (\sVal (llV, idx) -> buildInsertValue builder sVal llV idx "insertTuple") (LFFI.getUndef sType) (zip llVs [0..])

-- Basic vars
genVar (Num n) = return $ constReal doubleType (FFI.CDouble n)
genVar (Boolean b) = return $ constInt int1Type (FFI.fromBool b) False
genVar Unit = return $ constInt int1Type 0 False
genVar Time = curTimeVal <$> get
genVar v = errorDump [MkSB v] "NYI" assert


-- | Takes an already evaulated list of vars and processes the builtin op
-- can pattern match these directly as we know the types are all correct
genOp :: AC.Op -> [LLVM.Value] -> (LLVM.Builder -> MathOps -> String -> IO LLVM.Value)
-- Basic (Intrisic) Ops
genOp (AC.BasicOp AC.Add)   (v1:v2:[])  = (\b _ s -> buildFAdd b v1 v2 s)
genOp (AC.BasicOp AC.Sub)   (v1:v2:[])  = (\b _ s -> buildFSub b v1 v2 s)
genOp (AC.BasicOp AC.Mul)   (v1:v2:[])  = (\b _ s -> buildFMul b v1 v2 s)
genOp (AC.BasicOp AC.Div)   (v1:v2:[])  = (\b _ s -> buildFDiv b v1 v2 s)

genOp (AC.BasicOp AC.LT)    (v1:v2:[])  = (\b _ s -> buildFCmp b LFFI.FPOLT v1 v2 s)
genOp (AC.BasicOp AC.LE)    (v1:v2:[])  = (\b _ s -> buildFCmp b LFFI.FPOLE v1 v2 s)
genOp (AC.BasicOp AC.GT)    (v1:v2:[])  = (\b _ s -> buildFCmp b LFFI.FPOGT v1 v2 s)
genOp (AC.BasicOp AC.GE)    (v1:v2:[])  = (\b _ s -> buildFCmp b LFFI.FPOGE v1 v2 s)
genOp (AC.BasicOp AC.EQ)    (v1:v2:[])  = (\b _ s -> buildFCmp b LFFI.FPOEQ v1 v2 s)
genOp (AC.BasicOp AC.NEQ)   (v1:v2:[])  = (\b _ s -> buildFCmp b LFFI.FPONE v1 v2 s)

genOp (AC.BasicOp AC.And)   (v1:v2:[])  = (\b _ s -> buildAnd b v1 v2 s)
genOp (AC.BasicOp AC.Or)    (v1:v2:[])  = (\b _ s -> buildOr b v1 v2 s)
genOp (AC.BasicOp AC.Not)   (v1:[])     = (\b _ s -> buildNot b v1 s)
-- Math Ops - re-route to pre-defined LLVM func calls
genOp (AC.MathOp mOp)   vs = genMathCall mOp vs
genOp op vs = errorDump [MkSB op, MkSB vs] "Not implemented" assert

genMathCall mOp vs b opMap s = do
    v <- buildCall b (opMap Map.! mOp) vs s
    -- should we be using fastcc for ext libs?
    -- setInstructionCallConv v Fast
    setTailCall v True
    addInstrAttributes v [ReadNoneAttribute, NoUnwindAttribute]
    return v

-- Helper Funcs --------------------------------------------------------------------------------------------------------


