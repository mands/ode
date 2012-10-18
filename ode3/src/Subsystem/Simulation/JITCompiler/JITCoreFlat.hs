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

module Subsystem.Simulation.JITCompiler.JITCoreFlat (
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
    trace' [MkSB i, MkSB v] "Adding var to localmap" $ return ()
    return llV

-- process vs, then op
genExpr i (ExprData (Op op vs) t) = do
    vs' <- mapM genVar vs
    let buildOpF = genOp op vs'
    -- call the build op func
    GenState { builder, mathOps } <- get
    llV <- liftIO $ buildOpF builder mathOps (getValidIdName i)
    trace' [MkSB i, MkSB op] "Created op func" $ return ()
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
    trace' [MkSB i, MkSB tupIdx] "Lookup in a const tuple" $ return ()
    llTupleV <- lookupId i
    GenState {builder} <- get
    liftIO $ buildExtractValue builder llTupleV (fromIntegral $ tupIdx - 1) ""


-- simple map over the vars
-- can use const structs as only have pure values - no need to alloca the struct on the stack and store/load
genVar (Tuple vs) = do
    trace' [MkSB vs] "Building a const tuple" $ return ()
    llVs <- mapM genVar vs
    liftIO $ constStruct llVs False


-- Basic vars
genVar (Num n) = return $ constReal doubleType (FFI.CDouble n)
genVar (Boolean b) = return $ constInt int1Type (FFI.fromBool b) False
genVar Unit = return $ constInt int1Type 0 False
genVar Time = do
    GenState {llvmMod, builder} <- get
    timeVal <- liftIO $ fromJust <$> getNamedGlobal llvmMod "_simTime"
    liftIO $ buildLoad builder timeVal ""
genVar v = errorDump [] "NYI" assert


-- | Takes an already evaulated list of vars and processes the builtin op
-- can pattern match these directly as we know the types are all correct
genOp :: AC.Op -> [LLVM.Value] -> (LLVM.Builder -> MathOps -> String -> IO LLVM.Value)
-- Basic Ops
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
genOp (AC.MathOp mOp)   vs = (\b opMap s -> buildCall b (opMap Map.! mOp) vs s)
genOp op vs = errorDump [MkSB op, MkSB vs] "Not implemented" assert


-- Helper Funcs --------------------------------------------------------------------------------------------------------