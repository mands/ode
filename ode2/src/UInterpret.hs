-----------------------------------------------------------------------------
--
-- Module      :  UInterpret
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | An interpreter for the untyped \ -calc we use, very basic but allows for mult args, etc and should
-- | be very easy to develop
-- | can make monadic, CPS style, and others if required later
-- | ref Typed tagless interpreters, can ensure interpreaton will succeed
-- | can create correct semantics using interpreter
-- | Pure interpretor
--
-----------------------------------------------------------------------------

-- {-# LANGUAGE GADTs, ExistentialQuantification, PatternGuards, KindSignatures, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UInterpret (
evalFun, Interpret, runMyInterpret, IntState(..), Env(..),
) where

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import System.Random

import Utilities
import AST
import ExecutableAST

-- our interpreting monad, combines monads using mtl library
-- can derive it automatically
newtype Interpret a = Interpret { runInterpret :: StateT IntState (WriterT ValOut (Reader Env)) a }
    deriving (Monad, MonadReader Env, MonadWriter ValOut, MonadState IntState)

-- do we care about the return value of entry func?
-- need to switch the order of monads in order to remove value returning
runMyInterpret :: Interpret a -> IntState -> Env -> (IntState, ValOut)
runMyInterpret m state env = runReader (runWriterT ((execStateT (runInterpret m) state))) env

--runMyInterpret m state env = runReader (runWriterT ((runStateT (runInterpret m) state))) env
--runMyInterpret m s e = runReader . runWriterT . runStateT . runInterpret

--type Interpret = Reader Env

-- need to use state monad to store ode values and random gen during evaulation
-- should we store the random stdgen state here or simply a inf-list of rands?
-- how do we handle branching off to sub func? should be okay
data IntState = IntState { odeEnv :: OdeEnv, gen :: StdGen, cacheNorm :: Maybe NumTy }

-- helper functions to access random numbers
-- gets a uniform random number
getRandom :: Interpret NumTy
getRandom = do
    s <- get
    let (val, g') = random (gen s)
    put (s {gen = g'})
    return val

-- returns a z-value based standard normal random number, based on the box-muller transform of uniform randoms
-- Why doesn't caching work?? Perhaps Maybe can't be used inside other monads?? MonadTrans are wrong?
getStdNorm :: Interpret NumTy
getStdNorm = do
{-
    s <- get
    case (cacheNorm s) of
        Just y -> put (s {cacheNorm = Nothing}) >> return y
        Nothing -> boxM >>= (\(x,y) -> put (s {cacheNorm = Just y}) >> return x)
-}
{-
    s <- get
    case (cacheNorm s) of
        Just y -> do
            put (s {cacheNorm = Nothing})
            return y
        Nothing -> do
            (x,y) <- boxM
            put (s {cacheNorm = Just y})
            return x
-}
{-
    maybe
        (boxM >>= \(x,y) -> put (s {cacheNorm = Just y}) >> return x)
        (\y -> put (s {cacheNorm = Nothing}) >> return y)
        maybeY
-}
    boxM >>= (\(_, x) -> return x)
  where
    boxM :: Interpret (NumTy, NumTy)
    boxM = do
        u1 <- getRandom
        u2 <- getRandom
        let t1 = sqrt(-2 * log(u1))
        let t2 = 2*pi*u2
        return (t1*cos(t2), t1*sin(t2))

-- accessors for the ode state, only allows odes with unique names across entire file/model, need fix
getOde :: Id -> Interpret NumTy
getOde i = liftM (\s -> fromJust $ lookup i (odeEnv s)) get

putOde :: Id -> NumTy -> Interpret ()
putOde i v = do
    s <- get
    put (s {odeEnv = (i,v):(odeEnv s)})

-- use writer monad to store values we're gonna write to disk
-- switch to Data.Seq after time
type ValOut = [NumTy]
-- use reader monad to store current evaluation env
-- func env - is it just the model? do we have to pass this around everywhere?, exported from eAST as static
type ValEnv = [(Id, NumTy)]
data Env = Env { fEnv :: FunEnv, vEnv :: ValEnv, step :: NumTy }

-- helper lookup funcs
lookupFunc :: Id -> Env -> Function
lookupFunc i env = fromJust $ Map.lookup i (fEnv env)

lookupVal :: Id -> Env -> Maybe NumTy
lookupVal i env = lookup i (vEnv env)

lookupStep :: Env -> NumTy
lookupStep = step

-- main expression evaluater, does so in a fixed environment
evalExp :: NumExp -> Interpret NumTy
evalExp (Number x) = return x
evalExp (BinExp x op y) = liftM2 (numOp op) (evalExp x) (evalExp y)

-- we need to lookup within both the local environment, if this fails then lookup within the ode env
evalExp (ValueRef i) = do
    i' <- asks (lookupVal i)
    maybe (getOde i) (\x -> return x) i'

evalExp (If c t e) = do
    c' <- evalBool c
    if (c') then (evalExp t) else (evalExp e)

-- ode and sde expressions
-- ode calls, we first check the odeEnv to see if a value exists, if so perform the delta step,
-- if it doesn't then eval the init statement
evalExp (ODE i init e) = do
    odeVal <- getOde i
    -- if doesn't exist eval init, if does eval e
    --odeVal' <- maybe (return init) (evalOde) odeVal
    odeVal' <- evalOde odeVal
    -- store it in the state and writer monad
    putOde i odeVal'
    -- we write and return the preivous odeVal, having calculated the next state
    tell [odeVal]
    return odeVal
  where
    -- need the timestep size
    evalOde :: NumTy -> Interpret NumTy
    evalOde odeVal = liftM2 (\val step -> val * step + odeVal) (evalExp e) (asks lookupStep)

-- sde calls, pretty much similar to the ode set
evalExp (SDE i init e wein) = do
    odeVal <- getOde i
    -- if doesn't exist eval init, if does eval e
    -- odeVal' <- maybe (return init) (evalSde) odeVal
    odeVal' <- evalSde odeVal
    -- store it in the state and writer monad
    putOde i odeVal'
    -- we write and return the preivous odeVal, having calculated the next state
    tell [odeVal]
    return odeVal
  where
    evalSde :: NumTy -> Interpret NumTy
    evalSde odeVal = liftM2 (\a b -> odeVal + a + b) (evalDet e) (evalWeiner wein)
    -- need the timestep size
    evalDet :: NumExp -> Interpret NumTy
    evalDet d = liftM2 (\val step -> val * step) (evalExp d) (asks lookupStep)
    -- do we need to save previous weiner processes??
    evalWeiner :: NumExp -> Interpret NumTy
    evalWeiner w = liftM3 (\val step norm -> val * sqrt(step) * norm) (evalExp w) (asks lookupStep) getStdNorm

-- run-time function calls, all inlined for now, can potentially call any haskell function
evalExp (UnaryFunc f x) = liftM f (evalExp x)
evalExp (BinaryFunc f x y) = liftM2 f (evalExp x) (evalExp y)

-- number operations
numOp :: NumOp -> (NumTy -> NumTy -> NumTy)
numOp Add = (+)
numOp Sub = (-)
numOp Mul = (*)
numOp Div = (/)

-- boolean expressoin eval, need to make short-circuiting
evalBool :: BoolExp -> Interpret Bool
evalBool (LogExp a op b) = liftM2 (logOp op) (evalBool a) (evalBool b)
evalBool (RelExp a op b) = liftM2 (relOp op) (evalExp a) (evalExp b)

logOp :: LogOp -> (Bool -> Bool -> Bool)
logOp And = (&&)
logOp Or = (||)
--logOp Not = ??

relOp :: RelOp -> (NumTy -> NumTy -> Bool)
relOp AST.LE = (<=)
relOp AST.LT = (<)
relOp AST.EQ = (==)
relOp AST.GT = (>)
relOp AST.GE = (>=)

-- updates the env due to let bindings
evalEnv :: EnvExp -> Interpret [NumTy]
evalEnv (Let i e b) = do
    e' <- evalExp e
    local (\env -> env {vEnv = (i,e'):(vEnv env)}) (evalEnv b)

-- internal component calls, can return mult args
-- is call by value for internal func calls
evalEnv (IntFuncCall ids i args b) = do
    -- evaluates the args using the current env
    args' <- mapM evalExp args
    f <- asks (lookupFunc i)
    res <- evalFun f args'
    -- eval body in the new env
    local (\env -> env {vEnv = (zip ids res) ++ (vEnv env)}) (evalEnv b)

-- evaluate the expressions within the existing environment (created by `local` recursion)
evalEnv (RetEnv rets) = mapM evalExp rets

-- (int/component) function calls
-- maybe we merge functions into expressions ?
-- evalFun (Function _ ids body) args = runReader (evalEnv body) env
-- could runreader here and eval the function rather than closure it for later??
evalFun :: Function -> [NumTy] -> Interpret [NumTy]
evalFun (Function _ ids body) args =
    local (\env -> env {vEnv = zip ids args}) (evalEnv body)
