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

data GenState = GenState    { -- stateMap :: Map.Map Id String     -- a mapping from (state) ids to global vals in bitcode
                              localMap :: Map.Map Id LLVM.Value -- a mapping from local-def'd ids to their LLVM vals
                            , mathOps :: MathOps -- a mapping to all externally defined funcs
                            , libOps :: LibOps -- a mapping to all externally defined funcs
                            , builder :: LLVM.Builder           -- the current inst. builder
                            , llvmMod :: LLVM.Module
                            , curFunc :: LLVM.Value
                            , curTimeVal :: LLVM.Value
                            , simParams :: Sys.SimParams
                            } deriving (Show)


-- this is a bit hacky as we use a null pointer to represent the initial builder
mkGenState = GenState Map.empty Map.empty Map.empty nullPtr nullPtr nullPtr nullPtr


-- Helper Funcs --------------------------------------------------------------------------------------------------------

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
        ]

    createPureFunc name funcType = do
        f <- addFunction llvmMod name' funcType
        setFunctionCallConv f Fast
        addFuncAttributes f [NoUnwindAttribute, ReadNoneAttribute]
        return f
     where
        name' = if (L.get Sys.lMathModel p == Sys.FastMath && name `elem` finiteFuncs)
            then "__" ++ name ++ "_finite" else name

        finiteFuncs =   ["acos", "acosh", "asin", "atan2", "atanh", "cosh", "sinh", "exp10", "exp2"
                        , "exp", "log10", "log2", "log", "fmod", "hypot", "pow", "sqrt"]

    createReadOnlyFunc name funcType = do
        f <- addFunction llvmMod name funcType
        -- addFuncAttributes f [NoUnwindAttribute, ReadNoneAttribute]
        return f

    libOps :: [(String, IO LLVM.Value)]
    libOps =
        [ ("init",   addFunction llvmMod "init" (functionType voidType [] False))
        , ("shutdown",   addFunction llvmMod "shutdown" (functionType voidType [] False))
        , ("start_sim",   addFunction llvmMod "start_sim" (functionType voidType [pointerType int8Type 0] False))
        , ("end_sim",   addFunction llvmMod "end_sim" (functionType voidType [] False))
        , ("write_dbls", addFunction llvmMod "write_dbls" (functionType voidType [int32Type, pointerType doubleType 0] False))
        ]

-- LLVM Funcs ----------------------------------------------------------------------------------------------------------
-- TODO - move these into LLVM.Wrapper at some point
buildExtractValue builder val idx str = withCString str $ \cStr ->
    LFFI.buildExtractValue builder val idx cStr

constStruct :: [LLVM.Value] -> Bool -> IO LLVM.Value
constStruct llVs b = withArrayLen llVs $ \len ptr ->
    return $ LFFI.constStruct ptr (fromIntegral len) b

constInBoundsGEP :: LLVM.Value -> [LLVM.Value] -> IO LLVM.Value
constInBoundsGEP v llVs = withArrayLen llVs $ \len ptr ->
    LFFI.constInBoundsGEP v ptr (fromIntegral len)

constGEP :: LLVM.Value -> [LLVM.Value] -> IO LLVM.Value
constGEP v llVs = withArrayLen llVs $ \len ptr ->
    return $ LFFI.constGEP v ptr (fromIntegral len)

buildAlloca builder lType str = withCString str $ \cStr ->
    LFFI.buildAlloca builder lType cStr

buildAllocaWithInit builder initV lType str = do
    allocaV <- buildAlloca builder lType str
    buildStore builder initV allocaV
    return allocaV

constDouble d = constReal doubleType (CDouble d)
constInt' i = constInt int64Type (fromIntegral i) False

constInt32' :: Integral a => a -> LLVM.Value
constInt32' i = constInt int32Type (fromIntegral i) False

buildNoOp :: Builder -> IO LLVM.Value
buildNoOp builder = buildBitCast builder (constInt' 0) int64Type "noop"

addGlobalWithInit :: LLVM.Module -> LLVM.Value -> LLVM.Type -> String -> IO LLVM.Value
addGlobalWithInit mod initVal typ name = do
    gVal <- addGlobal mod typ name
    LFFI.setInitializer gVal initVal
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

-- WTF!!! why does (-1/4294967295) work as index??
addInstrAttributes :: LLVM.Value -> [Attribute] -> IO LLVM.Value
addInstrAttributes v attrs = mapM_ (\a -> LFFI.addInstrAttribute v (fromIntegral (0-1)) $ LFFI.fromAttribute a) attrs >> return v


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


-- LLVM Higher-Level Control Structures (limited power) ----------------------------------------------------------------
ifStmt :: (MonadIO m) => Builder -> LLVM.Value -> LLVM.Value -> (Builder -> m LLVM.Value) -> (Builder -> m LLVM.Value)
    -> m [(LLVM.Value, LLVM.BasicBlock)]
ifStmt builder curFunc condVal trueF falseF = do
    -- build the BBs
    trueBB  <- liftIO $ appendBasicBlock curFunc "if.true"
    falseBB <- liftIO $ appendBasicBlock curFunc "if.false"
    endBB   <- liftIO $ appendBasicBlock curFunc "if.end"

    -- gen the cond branch
    liftIO $ buildCondBr builder condVal trueBB falseBB

    -- create a bb for true
    liftIO $ positionAtEnd builder trueBB
    trueV <- trueF builder
    liftIO $ buildBr builder endBB

    -- create a bb for false
    liftIO $ positionAtEnd builder falseBB
    falseV <- falseF builder
    liftIO $ buildBr builder endBB

    -- create a bb for the end of the if
    liftIO $ positionAtEnd builder endBB
    return [(trueV, trueBB), (falseV, falseBB)]

-- TODO - are phis correct for loopStart
doWhileStmt :: (MonadIO m) => Builder -> LLVM.Value -> (Builder -> m LLVM.Value) -> (Builder -> LLVM.Value -> m LLVM.Value) -> m ()
doWhileStmt builder curFunc loopBody condF = do
    -- create do-loop bb's
    loopStartBB <- liftIO $ appendBasicBlock curFunc "doWhile.start"
    loopEndBB <- liftIO $ appendBasicBlock curFunc "doWhile.end"
    -- create and br to loop body
    liftIO $ buildBr builder loopStartBB
    liftIO $ positionAtEnd builder loopStartBB
    bodyV <- loopBody builder
    -- while loop test
    condV <- condF builder bodyV
    liftIO $ buildCondBr builder condV loopStartBB loopEndBB
    -- leave loop
    liftIO $ positionAtEnd builder loopEndBB


-- We create const strings as consts global w/ internal linkage
-- TODO - check this is correct
createConstString :: LLVM.Module -> String -> IO LLVM.Value
createConstString mod str = do
    strVal <- addGlobalWithInit mod (constString str False) (arrayType int8Type (fromIntegral $ length str + 1)) "localConstStr"
    setLinkage strVal LFFI.InternalLinkage
    LFFI.setGlobalConstant strVal True
    return strVal
