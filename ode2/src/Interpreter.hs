-----------------------------------------------------------------------------
--
-- Module      :  Interpreter
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Basic interpretor of the typed \ -calc derivative we've created, very simple to use
-- | can make monadic, CPS style, and others if required later
-- | ref Typed tagless interpreters, can ensure interpreaton will succeed
-- | can create correct semantics using interpreter
-- | interpret in the IO monad
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs, ExistentialQuantification, PatternGuards, KindSignatures, TypeFamilies #-}

module Interpreter (


) where

import Data.List
import Data.Maybe
import Control.Monad
import Utilities
import AST
import TypedAST
import qualified ModelAST as M

-- basic expression evaluator
evalExp :: ValEnv -> TExp a -> a
evalExp env (TDbl x) = x
evalExp env (TDblOp x op y) = (numOp op) (evalExp env x) (evalExp env y)

-- need to look and type val from env
-- how do we cast this correctly ?
evalExp env (TVar i) = tVal
  where
    --val = 3.0 :: Double
    tVal = fromJust $ castATValue theType =<<  lookup i env -- we know this lookup will succeed due to type-checking earlier
    --tVal =  fromJust $ lookup i env
    --fVal = test' tVal
    -- fVal = fromJust $ lookup i env -- we know this lookup will succeed due to type-checking earlier

evalExp env (TIf c t e) = if (evalBool env c) then evalExp env t else evalExp env e

-- gets function from env, gets evaled args,

evalExp env (TFunCall i args) = evalFun args' env f
  where
    args' = map (evalExp env) args
    tf = fromJust $ lookup i fenv
    f = case tf of (ATFun f t) -> f


--need to extend env and keep evaling
evalExp env (TLet i t e b) = evalExp ((i, t_v):env) b
  where
    t_v = ATValue (evalExp env e) t


castATValue :: TTyp a -> ATValue -> Maybe (a)
castATValue t (ATValue v s) = do
    Eq <- test t s
    return v

-- number operations
numOp :: NumOp -> (NumTy -> NumTy -> NumTy)
numOp Add = (+)
numOp Sub = (-)
numOp Mul = (*)
numOp Div = (/)

-- boolean expressoin eval
evalBool :: ValEnv -> BoolExpr -> Bool
evalBool env (LogExpr a op b) = (logOp op) (evalBool env a) (evalBool env b)
evalBool env (RelExpr a op b) = (relOp op) (evalExp env a) (evalExp env b)

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

-- we need to eval the lambda args and body separetly
-- maybe is a haskell function wrapper around the intended functionality, but then just code func directly
{-
evalFun :: ValEnv -> TFun (a-> b) -> b
evalFun env (TBody e) = evalExp env e
evalFun env (TLam i t f) =
    -- need to get the argument, add to env and then recusvively call evalFun
    evalFun env f
-}

class Compile a where
    type Returns a
    evalFun :: [Double] -> ValEnv -> TFun a -> Returns a

instance Compile Double where
    type Returns Double = Double
    evalFun args env (TBody e) =  evalExp env e
    -- TLam is not well typed

instance (Type a, Compile b) => Compile (a -> b) where
    type Returns (a-> b) = Returns b
    -- TBody is not well typed
    -- maybe need to dyanmically test TTDbl
    evalFun (x:xs) env (TLam i t e) = evalFun xs ((i, ATValue x TTDbl):env) e

-- environments
-- expressions
--type ExpEnv = [(Id, ATExp)]
type FunEnv = [(Id, ATFun)]

fenv :: FunEnv
fenv = []

-- should the env be of evaluated expressions, i.e. values?
-- existential, same as Dyn type, maybe all doubles/pairs ?
data ATValue = forall a. ATValue (a) (TTyp a)
type ValEnv = [(Id, ATValue)]

-- basic runtime env, a set of functions
-- need to create a set of primitive functions
-- e.g. sin/cos, etc., others?
-- maybe uses a run time env or subsume into exp env,
-- could inline into the typecheck stage and embed directly into ast, rather than lookup
testEnv =
    [("sin", ATValue (sin) (TTArr TTDbl TTDbl))
    ,("cos", ATValue (cos) (TTArr TTDbl TTDbl))
    ,("tan", ATValue (tan) (TTArr TTDbl TTDbl))
    ,("sinh", ATValue (sinh) (TTArr TTDbl TTDbl))
    ,("cosh", ATValue (cosh) (TTArr TTDbl TTDbl))
    ,("tanh", ATValue (tanh) (TTArr TTDbl TTDbl))
    ,("exp", ATValue (exp) (TTArr TTDbl TTDbl))
    ,("log", ATValue (log) (TTArr TTDbl TTDbl))
    -- bit slow pow function, ah well
    ,("pow", ATValue (\x y -> (^) x (round y)) (TTArr TTDbl (TTArr TTDbl TTDbl)))
    ]


-- how do we start, maybe use the simulate functions
-- model exists as a set of typed functions
startSim :: Model -> [M.Simulate] -> IO ()
startSim model sims = do
    -- for each simulate statement, get the inital function from the model and call it

    mapM_ runSim sims

    return ()

  where
    runSim :: M.Simulate -> IO ()
    runSim (M.Simulate comp param from to step sample filename) = do
        -- create our time list
        let time = [from,step..to]

        -- strictly fold over time (!) evaluating the entry function at each point
        let res = foldl' oneStep 0.0 time

        -- need to write results to disk here...

        return ()

    -- performs a simgle time step interpretation, should be in IO
    oneStep :: NumTy -> NumTy -> NumTy
    oneStep s t =
        -- need to call the entry function with the param "time"
        t

