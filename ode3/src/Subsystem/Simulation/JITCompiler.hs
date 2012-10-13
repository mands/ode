-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Simulation.JITCompiler
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Takes a CoreFlat AST and simulates is using an JIT Compiler with a Forward Euler
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Subsystem.Simulation.JITCompiler (
compile
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


-- Types ---------------------------------------------------------------------------------------------------------------

type ExtOps = Map.Map AC.MathOp LLVM.Value

newtype GenM a = GenM { runGenM :: (StateT GenState (MExceptIO)) a }
    deriving (Monad, MonadError String, MonadIO, MonadState GenState, Functor) -- , MonadTrans)

data GenState = GenState    { stateMap :: Map.Map Id String     -- a mapping from (state) ids to global vals in bitcode
                            , localMap :: Map.Map Id LLVM.Value -- a mapping from local-def'd ids to their LLVM vals
                            , extOps :: ExtOps -- a mapping to all externally defined funcs
                            , builder :: LLVM.Builder           -- the current inst. builder
                            , llvmMod :: LLVM.Module
                            , curFunc :: LLVM.Value
                            , simParams :: Sys.SimParams
                            } deriving (Show)


-- this is a bit hacky as we use a null pointer to represent the initial builder
mkGenState = GenState Map.empty Map.empty Map.empty FFI.nullPtr FFI.nullPtr FFI.nullPtr

-- Entry ---------------------------------------------------------------------------------------------------------------

compile :: CF.Module -> Sys.SysExceptIO ()
compile mod = do

    -- setup the default simulation state
    p <- Sys.getSysState Sys.lSimParams
    liftIO $ debugM "ode3.sim" $ "JIT Compiling Model"


    -- configure LLVM
    -- liftIO $ initializeNativeTarget
    lift $ runStateT (runGenM codeGen) $ mkGenState p


    liftIO $ debugM "ode3.sim" $ "Starting JIT Simulation"
    -- run the simulation
    liftIO $ runSimulation

    -- close any output files? (handle within LLVM code)
    liftIO $ debugM "ode3.sim" $ "JIT Simulation Complete"
    return ()
  where
    -- all code that runs in the SimM monad
    codeGen :: GenM ()
    codeGen = do
        createJITModule mod



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
    _ <- genModelInitials odeMod
    _ <- genModelLoop odeMod

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

genModelInitials :: CF.Module -> GenM ()
genModelInitials odeMod = do
    -- define the func
    GenState {llvmMod} <- get
    curFunc <- liftIO $ addFunction llvmMod "modelInitials" (functionType voidType [] False)
    builder <- liftIO $ createBuilder
    -- create the entry block & pos the builder
    entryBB <- liftIO $ appendBasicBlock curFunc "entry"
    liftIO $ positionAtEnd builder entryBB
    -- store the builder
    modify (\st -> st { builder, curFunc })
    -- add the insts (if exprMap not empty)
    unless (OrdMap.null (_initExprs odeMod)) (void $ genExprMap (_initExprs odeMod))
    -- return void
    r <- liftIO $ buildRetVoid builder
    liftIO $ disposeBuilder builder

genModelLoop :: CF.Module -> GenM ()
genModelLoop odeMod = do
    -- define the func
    GenState {llvmMod} <- get
    curFunc <- liftIO $ addFunction llvmMod "modelLoop" (functionType voidType [] False)
    -- create the entry block & pos the builder
    entryBB <- liftIO $ appendBasicBlock curFunc "entry"
    builder <- liftIO $ createBuilder
    liftIO $ positionAtEnd builder entryBB
    -- store the builder
    modify (\st -> st { builder, curFunc })
    -- add the insts (if exprMap not empty)
    unless (OrdMap.null (_loopExprs odeMod)) (void $ genExprMap (_loopExprs odeMod))
    -- return void
    r <- liftIO $ buildRetVoid builder
    liftIO $ disposeBuilder builder

-- TODO
-- genSolver, genInit, genShutdown ??

-- General Expr Generation ---------------------------------------------------------------------------------------------
-- Much of this code is a similar to the interpreter code, generating LLVM values rather than actualling simulating the model

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
    GenState { builder, extOps } <- get
    llV <- liftIO $ buildOpF builder extOps (getValidIdName i)
    trace' [MkSB i, MkSB op] "Created op func" $ return ()
    return llV

-- process the cond, then BBs for each branch and a phi to connect them
genExpr i (ExprData (If vB emT emF) t) = do
    -- gen the if test
    llVB <- genVar vB

    -- gen the bbs
    GenState { builder, curFunc } <- get
    trueBB <- liftIO $ appendBasicBlock curFunc "if_true"
    falseBB <- liftIO $ appendBasicBlock curFunc "if_false"
    endBB <- liftIO $ appendBasicBlock curFunc "if_end"

    -- gen the cond branch
    liftIO $ buildCondBr builder llVB trueBB falseBB

    -- create a bb for true
    liftIO $ positionAtEnd builder trueBB
    llTrueV <- genExprMap emT
    liftIO $ buildBr builder endBB

    -- create a bb for false
    liftIO $ positionAtEnd builder falseBB
    llFalseV <- genExprMap emF
    liftIO $ buildBr builder endBB

    -- use a phi to join them
    liftIO $ positionAtEnd builder endBB
    llPhi <- liftIO $ buildPhi builder (convertType t) (getValidIdName i)
    liftIO $ addIncoming llPhi [ (llTrueV, trueBB), (llFalseV, falseBB) ]
    return llPhi

-- | Gen a var operation
genVar :: Var -> GenM LLVM.Value
-- refs lookup in env
genVar (VarRef i) = lookupId i

-- TODO - tuples require LLVM structs
--genVar (TupleRef i tupIdx) = do
--    Tuple vs <- lookupId i
--    return $ vs !! (fromInteger $ tupIdx - 1)
-- simple map over the vars
-- simVar (Tuple vs) = Tuple <$> mapM simVar vs

genVar Time = do
    GenState {llvmMod, builder} <- get
    timeVal <- liftIO $ fromJust <$> getNamedGlobal llvmMod "_simTime"
    liftIO $ buildLoad builder timeVal ""

genVar (Num n) = return $ constReal doubleType (FFI.CDouble n)
genVar (Boolean b) = return $ constInt int1Type (FFI.fromBool b) False
genVar Unit = return $ constNull voidType
genVar v = errorDump [] "NYI" assert


-- | Takes an already evaulated list of vars and processes the builtin op
-- can pattern match these directly as we know the types are all correct
genOp :: AC.Op -> [LLVM.Value] -> (LLVM.Builder -> ExtOps -> String -> IO LLVM.Value)
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

lookupId :: Id -> GenM LLVM.Value
lookupId i = do
    -- first look in local env
    GenState {localMap, llvmMod} <- get
    case Map.lookup i localMap of
        Just v -> return v
        -- use fromJust as this can't fail
        Nothing -> errorDump [MkSB i, MkSB localMap] "can't find localval in map" assert -- liftIO $ fromJust <$> getNamedGlobal llvmMod valName


getValidIdName :: Id -> String
getValidIdName i = "odeVal" ++ (show i)

convertType :: CF.Type -> LLVM.Type
convertType (CF.TFloat) = doubleType
convertType (CF.TBool) = int1Type
convertType (CF.TUnit) = voidType
convertType (CF.TTuple ts) = errorDump [] "NYI" assert


-- | Define the basic math operations required by the code-generator
-- TODO - set the func attribs and calling convs
defineMathOps :: LLVM.Module ->  IO ExtOps
defineMathOps llvmMod = do
    ops <- mapM seqMathOps mathOps
    return $ Map.fromList ops
  where
    seqMathOps = (\(a, b) -> b >>= (\b -> return (a, b)))

    mathOps :: [(AC.MathOp, IO LLVM.Value)]
    mathOps =
        -- trig funcs
        [ (AC.Sin,      addFunction llvmMod "sin" (functionType doubleType [doubleType] False))
        , (AC.Cos,      addFunction llvmMod "cos" (functionType doubleType [doubleType] False))
        , (AC.Tan,      addFunction llvmMod "tan" (functionType doubleType [doubleType] False))
        , (AC.ASin,     addFunction llvmMod "asin" (functionType doubleType [doubleType] False))
        , (AC.ACos,     addFunction llvmMod "acos" (functionType doubleType [doubleType] False))
        , (AC.ATan,     addFunction llvmMod "atan" (functionType doubleType [doubleType] False))
        , (AC.ATan2,    addFunction llvmMod "atan2" (functionType doubleType [doubleType, doubleType] False))
        -- hyperbolics
        , (AC.SinH,     addFunction llvmMod "sinh" (functionType doubleType [doubleType] False))
        , (AC.CosH,     addFunction llvmMod "cosh" (functionType doubleType [doubleType] False))
        , (AC.TanH,     addFunction llvmMod "tanh" (functionType doubleType [doubleType] False))
        , (AC.ASinH,    addFunction llvmMod "asinh" (functionType doubleType [doubleType] False))
        , (AC.ACosH,    addFunction llvmMod "acosh" (functionType doubleType [doubleType] False))
        , (AC.ATanH,    addFunction llvmMod "atanh" (functionType doubleType [doubleType] False))
        -- logs/exps
        , (AC.Exp,      addFunction llvmMod "exp" (functionType doubleType [doubleType] False))
        , (AC.Log,      addFunction llvmMod "log" (functionType doubleType [doubleType] False))
        -- powers
        , (AC.Pow,      addFunction llvmMod "pow" (functionType doubleType [doubleType, doubleType] False))
        , (AC.Sqrt,     addFunction llvmMod "sqrt" (functionType doubleType [doubleType] False))
        , (AC.Cbrt,     addFunction llvmMod "cbrt" (functionType doubleType [doubleType] False))
        , (AC.Hypot,    addFunction llvmMod "hypot" (functionType doubleType [doubleType, doubleType] False))
        ]







