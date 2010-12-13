-----------------------------------------------------------------------------
--
-- Module      :  ExecutableAST
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | ExecutableAST represents the lower level executable structure based on
-- | conversion from the ModelAST
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module ExecutableAST (
    Model(..), FunEnv, OdeEnv, Function(..), NumExp(..), EnvExp(..), BoolExp(..),
    convertAST
) where

import Control.Monad.State
import Data.Foldable (foldlM, foldrM)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import Utilities
import AST
import qualified ModelAST as M
-- model converted from model AST into executable AST that defines the simulation process directly
-- more expressive than model ast, i.e. nested if-expr allows, i.e. turing complete
-- simplter than model AST, much lost semantic information
-- basically is untyped lambda calculus/functional-lite protolangauge

-- language statements
type FunEnv = Map.Map Id Function
type OdeEnv = [(Id, NumTy)]
data Model = Model [Simulate] FunEnv OdeEnv

-- functions at top level only, first order
data Function = Function { fName :: Id, fInputs :: [Id], fBody :: EnvExp } deriving Show

-- recusive let bindings allowed to introduce new variables
-- let binds to a sdingle value only, need speical let for comp calls as may return mul values
-- modify env, general flow
-- prob worth just converting to a list ?? maybe faster?
data EnvExp = Let Id NumExp EnvExp
            | IntFuncCall [Id] Id [NumExp] EnvExp
            | RetEnv [NumExp]
            deriving Show

-- represents basic numeric expressions in a functoinal-like setting
data NumExp = BinExp NumExp NumOp NumExp | Number NumTy | ValueRef Id
--          | ExtFuncCall Id NumExp -- should switch to inline funcs, faster
            | UnaryFunc (NumTy -> NumTy) NumExp
            | BinaryFunc (NumTy -> NumTy -> NumTy) NumExp NumExp
            | If BoolExp NumExp NumExp
            | ODE Id NumTy NumExp
            | SDE Id NumTy NumExp NumExp
            deriving Show

instance Show (NumTy -> NumTy) where
    show a = "Unary Func"

instance Show (NumTy -> NumTy -> NumTy) where
    show a = "Binary Func"

data BoolExp    = LogExp BoolExp LogOp BoolExp
                | RelExp NumExp RelOp NumExp
                deriving Show

-- state monad used during conversion to hold the ode/sde initial vals
type Convert a = State [(Id, NumTy)] a

-- conversion from model ast to exe ast, assume it can't fail
convertAST :: M.Model -> MExcept Model
convertAST (M.Model sims comps) = Right (Model sims comps' odeEnv)
  where
    compsS = foldlM convertComps Map.empty (Map.elems comps)
    convertComps funcs comp = convertComp comp >>=
        \comp' -> return $ Map.insert (M.cName comp) comp' funcs
    -- need to run state monad here with init value
    (comps', odeEnv) = runState compsS []

-- component toplevel, is converted to a \ -calc function
convertComp :: M.Component -> Convert Function
convertComp (M.Component name inputs outputs body) = do
    rets <- mapM toNumExp outputs
    envExp <- foldrM toEnvExp (RetEnv rets) body
    return $ Function name inputs envExp

toEnvExp :: M.CompStmt -> EnvExp -> Convert EnvExp
toEnvExp (M.ValueDef name val) env = toNumExp val >>= (\v -> return $ Let name v env)
toEnvExp (M.CompCallDef outs name ins) env = (mapM toNumExp ins) >>=
    \ins' -> return $ IntFuncCall outs name ins' env

-- | recursively converts an Expession from model form to executable form
toNumExp :: M.Expr -> Convert NumExp
toNumExp (M.BinExpr lhs binop rhs) = do
    lhs' <- toNumExp lhs
    rhs' <- toNumExp rhs
    return $ BinExp lhs' binop rhs'
toNumExp (M.Number n) = return $ Number n
-- | needs to function conversion to handle both internal and external function calls
toNumExp (M.FuncCall i exps) =
    if (length exps == 1)
    then (toNumExp (exps!!0) >>= \a1 -> return $ UnaryFunc (f uFuncs) a1)
    else (toNumExp (exps!!0) >>=
            \a1 -> toNumExp (exps!!1) >>=
            \a2 -> return $ BinaryFunc (f bFuncs) a1 a2)
  where
    f = fromJust . lookup i

toNumExp (M.ValueRef v) = return $ ValueRef v
-- | converts a case defintion into a set of nested if statements
toNumExp (M.CaseExp cases def) =
    toNumExp def >>= \def' -> foldrM createIf def' cases
  where
    createIf (c,t) exp = do
        c' <- toBoolExp c
        t' <- toNumExp t
        return $ If c' t' exp

-- need to create ids atomically, NOT ANYMORE, BUMMER!
-- but we can save the init values here
-- should we name-mangle the odes?, woudl complicate runtime as have to carry current func name around
toNumExp (M.ODE i init e) = do
--    i <- get
--    put (i+1)
    s <- get
    put $ (i,init):s
    e' <- toNumExp e
    return $ ODE i init e'
toNumExp (M.SDE i init e w) = do
--    i <- get
--    put (i+1)
    s <- get
    put $ (i,init):s
    e' <- toNumExp e
    w' <- toNumExp w
    return $ SDE i init e' w'

-- convert boolean expressions used in case/piecewise expressions
toBoolExp :: M.BoolExpr -> Convert BoolExp
toBoolExp (M.RelExpr e1 op e2) = do
    e1' <- (toNumExp e1)
    e2' <- (toNumExp e2)
    return $ RelExp e1' op e2'
toBoolExp (M.LogExpr b1 bop b2) = do
    b1' <- (toBoolExp b1)
    b2' <- (toBoolExp b2)
    return $ LogExp b1' bop b2'
