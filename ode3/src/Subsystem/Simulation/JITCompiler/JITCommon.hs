-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Simulation.JITCompiler.JITCommon
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Common code needed by the JIT Compiler subsystem
--
-----------------------------------------------------------------------------

module Subsystem.Simulation.JITCompiler.JITCommon
where


-- Labels
import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)

-- LLVM code
import LLVM.Wrapper.Core as LLVM
import qualified LLVM.Wrapper.BitWriter as LLVM
import qualified LLVM.Wrapper.ExecutionEngine as LLVM
import qualified LLVM.FFI.Core as LFFI
import qualified LLVM.FFI.BitReader as LFFI
import qualified LLVM.FFI.ExecutionEngine as LFFI


import Data.Int
import Data.Word
import Foreign
import Foreign.C

import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Utils.OrdMap as OrdMap

import Control.Monad.State
import Utils.MonadSupply

import Utils.CommonImports
import qualified Subsystem.SysState as Sys

import AST.Common as AC
import AST.CoreFlat as CF


-- Types ---------------------------------------------------------------------------------------------------------------

type MathOps = Map.Map AC.MathOp LLVM.Value
type LibOps = Map.Map String LLVM.Value
type ParamMap = OrdMap.OrdMap Id LLVM.Value

newtype GenM a = GenM { runGenM :: (StateT GenState (MExceptIO)) a }
    deriving (Monad, MonadError String, MonadIO, MonadState GenState, Functor) -- , MonadTrans)

data GenState = GenState    { -- stateMap :: Map.Map Id String  -- a mapping from (state) ids to global vals in bitcode
                              localMap :: Map.Map Id LLVM.Value -- a mapping from local-def'd ids to their LLVM vals
                            , mathOps :: MathOps                -- a mapping to all externally defined funcs
                            , libOps :: LibOps                  -- a mapping to all externally defined funcs
                            , builder :: LLVM.Builder           -- the current inst. builder
                            , llvmMod :: LLVM.Module
                            , curFunc :: LLVM.Value
                            , curTimeVal :: LLVM.Value
                            , curBB :: LLVM.BasicBlock
                            , simParams :: Sys.SimParams
                            } deriving (Show)

-- this is a bit hacky as we use a null pointer to represent the initial builder
mkGenState = GenState Map.empty Map.empty Map.empty nullPtr nullPtr nullPtr nullPtr nullPtr

-- Helper Funcs --------------------------------------------------------------------------------------------------------

setBB :: BasicBlock -> GenM ()
setBB bb = modify (\st -> st { curBB = bb })

getBB :: GenM BasicBlock
getBB = curBB <$> get


-- lookup an id both within current env, then global env
lookupId :: Id -> GenM LLVM.Value
lookupId i = do
    -- first look in local env
    GenState {localMap, llvmMod} <- get
    -- trace' [MkSB localMap] "Localmap of vals" $ return ()
    case Map.lookup i localMap of
        Just v -> return v
        -- use fromJust as this can't fail
        Nothing -> liftIO $ fromJust <$> getNamedGlobal llvmMod (getValidIdName i) -- errorDump [MkSB i, MkSB localMap] "can't find localval in map" assert --


getValidIdName :: Id -> String
getValidIdName i = "odeVal" ++ (show i)

convertType :: CF.Type -> LLVM.Type
convertType (CF.TFloat) = doubleType
convertType (CF.TBool) = int1Type
convertType (CF.TUnit) = int1Type
convertType (CF.TTuple ts) = structType (map convertType ts) False

-- | Define the basic math operations required by the code-generator
-- TODO - set the func attribs and calling convs
defineExtOps :: Sys.SimParams -> LLVM.Module ->  IO (MathOps, LibOps)
defineExtOps p llvmMod = do
    mathOps <- mapM seqOps mathOps
    libOps <- mapM seqOps libOps
    return $ (Map.fromList mathOps, Map.fromList libOps)
  where
    seqOps = (\(a, b) -> b >>= (\b -> return (a, b)))

    mathOps :: [(AC.MathOp, IO LLVM.Value)]
    mathOps =
        -- trig funcs
        [ (AC.Sin,      createPureFunc "sin" (functionType doubleType [doubleType] False))
        , (AC.Cos,      createPureFunc "cos" (functionType doubleType [doubleType] False))
        , (AC.Tan,      createPureFunc "tan" (functionType doubleType [doubleType] False))
        , (AC.ASin,     createPureFunc "asin" (functionType doubleType [doubleType] False))
        , (AC.ACos,     createPureFunc "acos" (functionType doubleType [doubleType] False))
        , (AC.ATan,     createPureFunc "atan" (functionType doubleType [doubleType] False))
        , (AC.ATan2,    createPureFunc "atan2" (functionType doubleType [doubleType, doubleType] False))
        -- hyperbolics
        , (AC.SinH,     createPureFunc "sinh" (functionType doubleType [doubleType] False))
        , (AC.CosH,     createPureFunc "cosh" (functionType doubleType [doubleType] False))
        , (AC.TanH,     createPureFunc "tanh" (functionType doubleType [doubleType] False))
        , (AC.ASinH,    createPureFunc "asinh" (functionType doubleType [doubleType] False))
        , (AC.ACosH,    createPureFunc "acosh" (functionType doubleType [doubleType] False))
        , (AC.ATanH,    createPureFunc "atanh" (functionType doubleType [doubleType] False))
        -- logs/exps
        , (AC.Exp,      createPureFunc "exp" (functionType doubleType [doubleType] False))
        , (AC.Log,      createPureFunc "log" (functionType doubleType [doubleType] False))
        -- powers
        , (AC.Pow,      createPureFunc "pow" (functionType doubleType [doubleType, doubleType] False))
        , (AC.Sqrt,     createPureFunc "sqrt" (functionType doubleType [doubleType] False))
        , (AC.Cbrt,     createPureFunc "cbrt" (functionType doubleType [doubleType] False))
        , (AC.Hypot,    createPureFunc "hypot" (functionType doubleType [doubleType, doubleType] False))
        -- basic floating point ops
        , (AC.FAbs,     createPureFunc "fabs" (functionType doubleType [doubleType] False))
        , (AC.Floor,    createPureFunc "floor" (functionType doubleType [doubleType] False))
        , (AC.Ceil,     createPureFunc "ceil" (functionType doubleType [doubleType] False))
        , (AC.Round,    createPureFunc "round" (functionType doubleType [doubleType] False))
        ]

    createPureFunc name funcType = do
        f <- addFunction llvmMod name funcType
        -- setFunctionCallConv f Fast -- we shouldn't use fastcc as they are external funcs
        addFuncAttributes f [NoUnwindAttribute, ReadNoneAttribute]
        return f

--     where
-- we don't convert to finite funcs here, instead rely on vecmath to handle this externally
--        name' = if (L.get Sys.lMathModel p == Sys.Fast && Set.member name finiteFuncs)
--            then "__" ++ name ++ "_finite" else name
--
--    finiteFuncs = Set.fromList  ["acos", "acosh", "asin", "atan2", "atanh", "cosh", "sinh", "exp10", "exp2"
--                                , "exp", "log10", "log2", "log", "fmod", "hypot", "pow", "sqrt"]

    createReadOnlyFunc' name funcType = do
        f <- addFunction llvmMod name funcType
        addFuncAttributes f [NoUnwindAttribute, ReadOnlyAttribute]
        return f

    -- hardcoded interface to the Ode Stdlib
    libOps :: [(String, IO LLVM.Value)]
    libOps =
        [ ("OdeInit",       addFunction llvmMod "OdeInit" (functionType voidType [] False))
        , ("OdeShutdown",   addFunction llvmMod "OdeShutdown" (functionType voidType [] False))
        , ("OdeStartSim",   do
                                f <- addFunction llvmMod "OdeStartSim" (functionType voidType [pointerType int8Type 0, int64Type] False)
                                setFuncParam f 0 [NoAliasAttribute, NoCaptureAttribute]
                                return f
                                )
        , ("OdeStopSim",     addFunction llvmMod "OdeStopSim" (functionType voidType [] False))
        , ("OdeWriteState", do
                                f <- addFunction llvmMod "OdeWriteState" (functionType voidType [doubleType, pointerType doubleType 0] False)
                                setFuncParam f 1 [NoAliasAttribute, NoCaptureAttribute]
                                return f
                                )
        ]


-- | A helper function that performs much of the boiler-plate code in creating a function
genFunction :: String -> LLVM.Type -> [LLVM.Type] -> GenM (LLVM.Value, LLVM.Builder)
genFunction fName fRetType fArgTypes = do
    -- define the func
    GenState {llvmMod} <- get
    curFunc <- liftIO $ addFunction llvmMod fName (functionType fRetType fArgTypes False)
    builder <- liftIO $ createBuilder
    -- create the entry block & pos the builder
    curBB <- liftIO $ appendBasicBlock curFunc "entry"
    liftIO $ positionAtEnd builder curBB
    -- setup state and return (func, builder)
    modify (\st -> st { builder, curFunc, localMap = Map.empty, curBB })
    return (curFunc, builder)


-- LLVM Funcs ----------------------------------------------------------------------------------------------------------
-- TODO - move these into LLVM.Wrapper at some point
setTarget mod str = withCString str $ \cStr ->
    LFFI.setTarget mod cStr

buildExtractValue builder val idx str = withCString str $ \cStr ->
    LFFI.buildExtractValue builder val idx cStr

buildInsertValue builder aggVal eleVal idx str = withCString str $ \cStr ->
    LFFI.buildInsertValue builder aggVal eleVal idx cStr

constStruct :: [LLVM.Value] -> Bool -> IO LLVM.Value
constStruct llVs b = withArrayLen llVs $ \len ptr ->
    return $ LFFI.constStruct ptr (fromIntegral len) b

constInBoundsGEP :: LLVM.Value -> [LLVM.Value] -> IO LLVM.Value
constInBoundsGEP v llVs = withArrayLen llVs $ \len ptr ->
    LFFI.constInBoundsGEP v ptr (fromIntegral len)

constGEP :: LLVM.Value -> [LLVM.Value] -> IO LLVM.Value
constGEP v llVs = withArrayLen llVs $ \len ptr ->
    return $ LFFI.constGEP v ptr (fromIntegral len)

buildAllocaWithInit builder initV lType str = do
    allocaV <- buildAlloca builder lType str
    buildStore builder initV allocaV
    return allocaV

-- const value wrappers
constDouble d = constReal doubleType (CDouble d)
constInt64 i = constInt int64Type (fromIntegral i) False
constInt32 i = constInt int32Type (fromIntegral i) False

constArray :: LLVM.Type -> [LLVM.Value] -> IO LLVM.Value
constArray t llVs = withArrayLen llVs $ \len ptr ->
    return $ LFFI.constArray t ptr (fromIntegral len)

-- | LLVM No-Op - sometimes needed for funcs a LLVM value is required (i.e. FP-style if stmts)
buildNoOp :: Builder -> IO LLVM.Value
buildNoOp builder = buildBitCast builder (constInt64 0) int64Type "noop"

-- | Add a global value, with initial value, to the module that may be set as constant
addGlobalWithInit :: LLVM.Module -> LLVM.Value -> LLVM.Type -> Bool -> String -> IO LLVM.Value
addGlobalWithInit mod initVal typ isConst name = do
    gVal <- addGlobal mod typ name
    LFFI.setInitializer gVal initVal
    LFFI.setGlobalConstant gVal isConst
    return gVal

updatePtrVal :: Builder -> LLVM.Value -> (LLVM.Value -> IO LLVM.Value) -> IO ()
updatePtrVal builder ptrVal updateFunc = do
    val' <- withPtrVal builder ptrVal updateFunc
    _ <- buildStore builder val' ptrVal
    return ()

withPtrVal :: Builder -> LLVM.Value -> (LLVM.Value -> IO a) -> IO a
withPtrVal builder ptrVal runFunc = buildLoad builder ptrVal "derefVal" >>= (\val -> runFunc val)

addParamAttributes :: LLVM.Value -> [Attribute] -> IO LLVM.Value
addParamAttributes v attrs = mapM_ (addAttribute v) attrs >> return v

addFuncAttributes :: LLVM.Value -> [Attribute] -> IO LLVM.Value
addFuncAttributes v attrs = mapM_ (addFunctionAttr v) attrs >> return v

-- WTF!!! why does (-1 or 4294967295) work as the index??
addInstrAttributes :: LLVM.Value -> [Attribute] -> IO LLVM.Value
addInstrAttributes v attrs = mapM_ (\a -> LFFI.addInstrAttribute v (fromIntegral (0-1)) $ LFFI.fromAttribute a) attrs >> return v

setFuncParam :: LLVM.Value -> Int -> [Attribute] -> IO ()
setFuncParam f idx attrs = do
    ps <- getParams f
    _ <- addParamAttributes (ps !! idx) attrs
    return ()


--runFunction' :: LLVM.ExecutionEngine -> LLVM.Value -> [LFFI.GenericValue] -> IO LFFI.GenericValue
--runFunction' ee f args
--    = withArrayLen args $ \numArgs ptr -> LFFI.runFunction ee f numArgs ptr

-- |Read a module from a file (taken from LLVM High-level bindings)
readBitcodeFromFile :: String -> IO LLVM.Module
readBitcodeFromFile name =
    withCString name $ \ namePtr ->
      alloca $ \ bufPtr ->
      alloca $ \ modPtr ->
      alloca $ \ errStr -> do
        rrc <- LFFI.createMemoryBufferWithContentsOfFile namePtr bufPtr errStr
        if rrc /= False then do
            msg <- peek errStr >>= peekCString
            ioError $ userError $ "readBitcodeFromFile: read return code " ++ show rrc ++ ", " ++ msg
         else do
            buf <- peek bufPtr
            prc <- LFFI.parseBitcode buf modPtr errStr
            if prc /= False then do
                msg <- peek errStr >>= peekCString
                ioError $ userError $ "readBitcodeFromFile: parse return code " ++ show prc ++ ", " ++ msg
             else peek modPtr


-- LLVM Higher-Level Control Structures (limited power, useful for FP langs) -------------------------------------------

-- | An FP-style If-stmt, ie. requires both branches to be present, return types of each must be equal
-- fixed to handle sub-BBs in each branch, needs to use GenM monad for now
ifStmt :: Builder -> LLVM.Value -> LLVM.Value -> (Builder -> GenM LLVM.Value) -> (Builder -> GenM LLVM.Value)
    -> GenM [(LLVM.Value, LLVM.BasicBlock)]
ifStmt builder curFunc condVal trueF falseF = do
    -- build the BBs
    trueBB  <- liftIO $ appendBasicBlock curFunc "if.true"
    falseBB <- liftIO $ appendBasicBlock curFunc "if.false"
    endBB   <- liftIO $ appendBasicBlock curFunc "if.end"

    -- gen the cond branch
    liftIO $ buildCondBr builder condVal trueBB falseBB

    -- create a bb for true
    liftIO $ positionAtEnd builder trueBB
    setBB trueBB
    trueV <- trueF builder
    trueBB' <- getBB
    liftIO $ buildBr builder endBB

    -- create a bb for false
    liftIO $ positionAtEnd builder falseBB
    setBB falseBB
    falseV <- falseF builder
    falseBB' <- getBB
    liftIO $ buildBr builder endBB

    -- create a bb for the end of the if
    liftIO $ positionAtEnd builder endBB
    setBB endBB
    -- let the caller deal with the phis
    return [(trueV, trueBB'), (falseV, falseBB')]

-- | A basic do-While loop
-- TODO - are phis correct for loopStart - yes, think so
doWhileStmt :: Builder -> LLVM.Value -> (Builder -> GenM LLVM.Value) -> (Builder -> LLVM.Value -> GenM LLVM.Value) -> GenM ()
doWhileStmt builder curFunc doBodyF doCondF = do
    -- create do-loop bb's
    doBodyBB <- liftIO $ appendBasicBlock curFunc "do.body"
    doCondBB <- liftIO $ appendBasicBlock curFunc "do.cond"
    doEndBB <- liftIO $ appendBasicBlock curFunc "do.end"

    -- create and br to loop body
    liftIO $ buildBr builder doBodyBB
    liftIO $ positionAtEnd builder doBodyBB
    setBB doBodyBB
    bodyV <- doBodyF builder

    -- while loop test
    liftIO $ buildBr builder doCondBB
    liftIO $ positionAtEnd builder doCondBB
    setBB doCondBB
    condV <- doCondF builder bodyV
    liftIO $ buildCondBr builder condV doBodyBB doEndBB

    -- leave loop
    liftIO $ positionAtEnd builder doEndBB
    setBB doEndBB

