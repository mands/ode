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
                            , simParams :: Sys.SimParams
                            } deriving (Show)


-- this is a bit hacky as we use a null pointer to represent the initial builder
mkGenState = GenState Map.empty Map.empty Map.empty FFI.nullPtr FFI.nullPtr

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

    -- insert the global vals - both states and deltas
    _ <- liftIO $ addGlobal llvmMod (doubleType) "testVal"
    _ <- liftIO $ addGlobal llvmMod (doubleType) "delta_testVal"

    -- insert the funcs into the module
    _ <- genModelInitials llvmMod odeMod
    _ <- genModelLoop llvmMod odeMod

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

genModelInitials :: LLVM.Module -> CF.Module -> GenM ()
genModelInitials llvmMod odeMod = do
    -- define the func
    f <- liftIO $ addFunction llvmMod "modelInitials" (functionType voidType [] False)
    builder <- liftIO $ createBuilder
    -- create the entry block & pos the builder
    entryBB <- liftIO $ appendBasicBlock f "entry"
    liftIO $ positionAtEnd builder entryBB
    -- store the builder
    modify (\st -> st { builder })
    -- add the insts (if exprMap not empty)
    unless (OrdMap.null (_initExprs odeMod)) (void $ genExprMap (_initExprs odeMod))
    -- return void
    r <- liftIO $ buildRetVoid builder
    liftIO $ disposeBuilder builder

genModelLoop :: LLVM.Module -> CF.Module -> GenM ()
genModelLoop llvmMod odeMod = do
    -- define the func
    f <- liftIO $ addFunction llvmMod "modelLoop" (functionType voidType [] False)
    -- create the entry block & pos the builder
    entryBB <- liftIO $ appendBasicBlock f "entry"
    builder <- liftIO $ createBuilder
    liftIO $ positionAtEnd builder entryBB
    -- store the builder
    modify (\st -> st { builder })
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
        -- GenState { builder } <- get
        -- create a local val using an alloca
        -- valV <- liftIO $ FFI.withCString (getValidIdName i) (LFFI.buildAlloca builder exprT)
        -- store the val in the alloca
        -- storeV <- liftIO $ buildStore builder exprV valV
        -- add the value to the scope map
        -- modify (\(GenState {..}) -> GenState {localMap = Map.insert i valV localMap, .. })
        -- modify (\st@(GenState {localMap}) -> st {localMap = Map.insert i valV localMap })
        return llV

-- | Generate a single expression
-- we both generate the expr and store it here locally
genExpr :: Id -> ExprData -> GenM (LLVM.Value)
genExpr i (ExprData (Var v) t) = do
    -- need to allocate an expr, depends on the type and value generated
    llV <- genVar v
    -- add the value to the scope map
    modify (\st@(GenState {localMap}) -> st {localMap = Map.insert i llV localMap })
    trace' [MkSB i, MkSB v] "Adding var to localmap" $ return ()
    -- isCon <- liftIO $ LLVM_FFI.isConstant v'
--    if isCon
--        then do
--            -- create a static call
--            return ()
--        else do
--            -- create an alloca
--            return ()

    -- should be an alloca
    return llV

-- process vs, then op
genExpr i (ExprData (Op op vs) t) = do
    vs' <- mapM genVar vs
    let buildOpF = genOp op vs'
    -- call the build op func
    GenState { builder, extOps } <- get
    llV <- liftIO $ buildOpF builder extOps (getValidIdName i)
    -- store the builder?
    -- modify (\st -> st { builder })

    -- add the value to the scope map
    modify (\st@(GenState {localMap}) -> st {localMap = Map.insert i llV localMap })

    trace' [MkSB i, MkSB op] "Created op func" $ return ()
    return llV

-- TODO - need to do some funky phi ops for Ifs
genExpr i (ExprData (If vB emT emF) t) = undefined


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

-- lookup in env
-- genVar v@Time = Num <$> (_curTime <$> get)
-- any other vars (will be literals) are just copied across
-- genVar v = return v

genVar (Num n) = return $ constReal doubleType (FFI.CDouble n)
genVar (Boolean b) = return $ constInt int1Type (FFI.fromBool b) False
-- TODO - how do we pass unit values across? as nullptr/0/false?
genVar Unit = errorDump [] "NYI" assert

genVar v = errorDump [] "NYI" assert

lookupId :: Id -> GenM LLVM.Value
lookupId i = do
    -- first look in local env
    GenState {localMap, llvmMod} <- get
    case Map.lookup i localMap of
        Just v -> return v
        -- use fromJust as this can't fail
        Nothing -> errorDump [MkSB i, MkSB localMap] "can't find localval in map" assert -- liftIO $ fromJust <$> getNamedGlobal llvmMod valName
  where
    valName = getValidIdName i


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

-- Math Ops
genOp (AC.MathOp mOp)   vs = (\b opMap s -> buildCall b (opMap Map.! mOp) vs s)

genOp op vs = errorDump [MkSB op, MkSB vs] "Not implemented" assert
--    v <- liftIO $ buildAdd builder (constInt int64Type 1 False) (constInt int64Type 2 False) "tmp"
--    liftIO $ buildRet builder v



-- Helper Funcs --------------------------------------------------------------------------------------------------------

getValidIdName :: Id -> String
getValidIdName i = "odeVal" ++ (show i)

convertType :: CF.Type -> LLVM.Type
convertType (CF.TFloat) = doubleType
convertType (CF.TBool) = int1Type
convertType (CF.TUnit) = errorDump [] "NYI" assert
convertType (CF.TTuple ts) = errorDump [] "NYI" assert

defineMathOps :: LLVM.Module ->  IO ExtOps
defineMathOps llvmMod = do
    ops <- mapM seqMathOps mathOps
    return $ Map.fromList ops
  where
    seqMathOps = (\(a, b) -> b >>= (\b -> return (a, b)))

    mathOps :: [(AC.MathOp, IO LLVM.Value)]
    mathOps =
        [ (AC.Sin, addFunction llvmMod "sin" (functionType doubleType [doubleType] False))
        , (AC.Cos, addFunction llvmMod "cos" (functionType doubleType [doubleType] False))
        ]







