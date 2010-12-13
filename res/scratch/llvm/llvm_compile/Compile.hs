{-# LANGUAGE TypeFamilies, FlexibleContexts, ExistentialQuantification, FlexibleInstances, UndecidableInstances, GADTs #-}
module Compile(Translate, translate) where
import Data.Maybe
import Prelude hiding (and, or)
import TExp
import LLVM.Core hiding (CmpOp)
import LLVM.ExecutionEngine
import System.IO.Unsafe(unsafePerformIO)

-- recursively compiles an expression
compileExp :: (Type a, IsFirstClass a) => Env -> TExp a -> CodeGenFunction r (Value a)
compileExp _ (TDbl d) = return $ valueOf d
compileExp _ (TBol b) = return $ valueOf b
compileExp r (TDblOp op e1 e2) = bind2 (dblOp op) (compileExp r e1) (compileExp r e2)
compileExp r (TBolOp op e1 e2) = bind2 (bolOp op) (compileExp r e1) (compileExp r e2)
compileExp r (TCmpOp op e1 e2) = bind2 (cmpOp op) (compileExp r e1) (compileExp r e2)
compileExp r (TIf b t e) = mkIf (compileExp r b) (compileExp r t) (compileExp r e)
compileExp r (TLet i t e b) = do
    e' <- compileExp' r t e
    compileExp ((i, AValue e' t):r) b
compileExp r (TVar i) = return $ fromJust $ castAValue theType =<< lookup i r   -- lookup cannot fail on type checked code

compileExp' :: Env -> TTyp a -> TExp a -> CodeGenFunction r (Value a)
compileExp' r TTDbl e = compileExp r e
compileExp' r TTBol e = compileExp r e
compileExp' _ _ _ = error $ "compileExp': functions not allowed yet"

data AValue = forall a . AValue (Value a) (TTyp a)

castAValue :: TTyp a -> AValue -> Maybe (Value a)
castAValue t (AValue v s) = do
    Eq <- test t s
    return v

type Env = [(Id, AValue)]



dblOp :: DblOp -> Value Double -> Value Double -> CodeGenFunction r (Value Double)
dblOp DAdd = add
dblOp DSub = sub
dblOp DMul = mul
dblOp DDiv = fdiv

-- This should be in Control.Monad
bind2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bind2 f m1 m2 = do
    x1 <- m1
    x2 <- m2
    f x1 x2
    
bolOp :: BolOp -> Value Bool -> Value Bool -> CodeGenFunction r (Value Bool)
bolOp BAnd = and
bolOp BOr  = or

cmpOp :: CmpOp -> Value Double -> Value Double -> CodeGenFunction r (Value Bool)
cmpOp CEq = fcmp FPOEQ
cmpOp CLe = fcmp FPOLE

mkIf :: (IsFirstClass a) =>
        CodeGenFunction r (Value Bool) -> CodeGenFunction r (Value a) -> CodeGenFunction r (Value a) -> CodeGenFunction r (Value a)
mkIf mb mt me = do
    b <- mb
    tb <- newBasicBlock
    eb <- newBasicBlock
    jb <- newBasicBlock
    condBr b tb eb
    defineBasicBlock tb
    t <- mt
    br jb
    defineBasicBlock eb
    e <- me
    br jb
    defineBasicBlock jb
    phi [(t, tb), (e, eb)]
    

-- | Compile a TFun into the corresponding LLVM code.
compileFunction :: (Translate a) =>
                   TFun a -> CodeGenModule (Function (RetIO a))
compileFunction = createFunction ExternalLinkage . compileFun []

class Compile a where
    type CG a
    type RetIO a
    type Returns a
    compileFun :: Env -> TFun a -> CG a

instance Compile Double where
    type CG Double = CodeGenFunction Double ()
    type RetIO Double = IO Double
    type Returns Double = Double
    compileFun r (TBody e) = compileExp r e >>= ret
    -- TLam is not well typed

instance Compile Bool where
    type CG Bool = CodeGenFunction Bool ()
    type RetIO Bool = IO Bool
    type Returns Bool = Bool
    compileFun r (TBody e) = compileExp r e >>= ret
    -- TLam is not well typed

instance (Type a, Compile b) => Compile (a -> b) where
    type CG (a->b) = Value a -> CG b
    type RetIO (a->b) = a -> RetIO b
    type Returns (a->b) = Returns b
    -- TBody is not well typed
    compileFun r (TLam i t e) = \ x -> compileFun ((i, AValue x t):r) e

class    (Type a,
          Unsafe (RetIO a) a,
          FunctionArgs (RetIO a) (CG a) (CodeGenFunction (Returns a) ()),
          IsFunction (RetIO a),
          Compile a,
          Translatable (RetIO a)) =>
    Translate a
instance (Type a,
          Unsafe (RetIO a) a,
          FunctionArgs (RetIO a) (CG a) (CodeGenFunction (Returns a) ()),
          IsFunction (RetIO a),
          Compile a,
          Translatable (RetIO a)) =>
    Translate a

translate :: (Translate a) => TFun a -> a
translate = unsafePerformIO . fmap unsafePurify . simpleFunction . compileFunction

