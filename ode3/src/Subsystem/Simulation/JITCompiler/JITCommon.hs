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


-- Types ---------------------------------------------------------------------------------------------------------------

type MathOps = Map.Map AC.MathOp LLVM.Value
type LibOps = Map.Map String LLVM.Value
type ParamMap = OrdMap.OrdMap Id LLVM.Value

newtype GenM a = GenM { runGenM :: (StateT GenState (MExceptIO)) a }
    deriving (Monad, MonadError String, MonadIO, MonadState GenState, Functor) -- , MonadTrans)

data GenState = GenState    { stateMap :: Map.Map Id String     -- a mapping from (state) ids to global vals in bitcode
                            , localMap :: Map.Map Id LLVM.Value -- a mapping from local-def'd ids to their LLVM vals
                            , mathOps :: MathOps -- a mapping to all externally defined funcs
                            , libOps :: LibOps -- a mapping to all externally defined funcs
                            , builder :: LLVM.Builder           -- the current inst. builder
                            , llvmMod :: LLVM.Module
                            , curFunc :: LLVM.Value
                            , simParams :: Sys.SimParams
                            } deriving (Show)


-- this is a bit hacky as we use a null pointer to represent the initial builder
mkGenState = GenState Map.empty Map.empty Map.empty Map.empty FFI.nullPtr FFI.nullPtr FFI.nullPtr


-- Helper Funcs --------------------------------------------------------------------------------------------------------

lookupId :: Id -> GenM LLVM.Value
lookupId i = do
    -- first look in local env
    GenState {localMap, llvmMod} <- get
    trace' [MkSB localMap] "Localmap of vals" $ return ()
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
defineExtOps :: LLVM.Module ->  IO (MathOps, LibOps)
defineExtOps llvmMod = do
    mathOps <- mapM seqOps mathOps
    libOps <- mapM seqOps libOps
    return $ (Map.fromList mathOps, Map.fromList libOps)
  where
    seqOps = (\(a, b) -> b >>= (\b -> return (a, b)))

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


    libOps :: [(String, IO LLVM.Value)]
    libOps =
        [ ("init",   addFunction llvmMod "init" (functionType voidType [] False))
        , ("shutdown",   addFunction llvmMod "shutdown" (functionType voidType [] False))
        , ("start_sim",   addFunction llvmMod "start_sim" (functionType voidType [pointerType int8Type 0] False))
        , ("end_sim",   addFunction llvmMod "end_sim" (functionType voidType [] False))
        , ("write_dbls", addFunction llvmMod "write_dbls" (functionType voidType
            [int32Type, pointerType doubleType 0] False))
        ]


-- LLVM Funcs ----------------------------------------------------------------------------------------------------------
-- TODO - move these into LLVM.Wrapper at some point
buildExtractValue builder val idx str = FFI.withCString str $ \cStr ->
    LFFI.buildExtractValue builder val idx cStr

constStruct :: [LLVM.Value] -> Bool -> IO LLVM.Value
constStruct llVs b = FFI.withArrayLen llVs $ \len ptr ->
    return $ LFFI.constStruct ptr (fromIntegral len) b

constInBoundsGEP :: LLVM.Value -> [LLVM.Value] -> IO LLVM.Value
constInBoundsGEP v llVs = FFI.withArrayLen llVs $ \len ptr ->
    LFFI.constInBoundsGEP v ptr (fromIntegral len)

constGEP :: LLVM.Value -> [LLVM.Value] -> IO LLVM.Value
constGEP v llVs = FFI.withArrayLen llVs $ \len ptr ->
    return $ LFFI.constGEP v ptr (fromIntegral len)

buildAlloca builder lType str = FFI.withCString str $ \cStr ->
    LFFI.buildAlloca builder lType cStr

buildAllocaWithInit builder initV lType str = do
    allocaV <- buildAlloca builder lType str
    buildStore builder initV allocaV
    return allocaV

constDouble d = constReal doubleType (FFI.CDouble d)
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
    loopStartBB <- liftIO $ appendBasicBlock curFunc "doWhile.Start"
    loopEndBB <- liftIO $ appendBasicBlock curFunc "doWhile.End"
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
    strVal <- addGlobalWithInit mod (constString str True) (arrayType int8Type (fromIntegral $ length str)) "localStr"
    LFFI.setInitializer strVal $ constString str True
    setLinkage strVal LFFI.InternalLinkage
    LFFI.setGlobalConstant strVal True
    return strVal
