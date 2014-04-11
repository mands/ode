-----------------------------------------------------------------------------
--
-- Module      :  Core.Expr.AST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | A lower-level desuagred AST that represents the mian language features
-- is parameterised by the types of values
-- a reference interpreter exists that may execute on the type-checked Core AST
-- bascially the lambda-calculus style IR
-- should be a functor with fmap defined
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies  #-}
{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures, DataKinds #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}


module AST.Core (
VarId(..), BindList,
Type(..), mapTypeM, mapType, addLabels, dropLabels,
TopLet(..), Expr(..), Op(..), Literal(..), TypeCast(..),
mapExpr, mapExprM, foldExpr, foldExprM,

SrcId, DesId, Id, RecId, -- rexported from Common.AST
) where

import Prelude hiding (LT, GT, EQ)
import Control.Monad

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.Maybe (fromJust, isJust)
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import Data.Functor

import Utils.CommonImports
import AST.Common as AC
import qualified Subsystem.Units as U

-- | DetailId - holds both a (parameterised) identifier and a string that represetns the (closest) original/source file, variable and line num
--data DetailId a = DetailId a SrcId Int deriving (Show, Eq, Ord)

-- Types ------------------------------------------------------------------------------------------------------------

-- | Types
data Type :: * where
    TVar :: Integer -> Type
    TBool :: Type
    TFloat :: U.Unit -> Type
    TUnit :: Type
    -- Composite Types
    TArr :: Type -> Type -> Type
    -- We use ModFullName as a common ID between TTypeCons and TWrap in order to check they types are valid
    -- NOTE - don't need to add SrcId as ID too, as duplicate bindings not allowed within module
    TTypeCons :: ModFullName -> Type -> Type
    TWrap :: ModFullName -> Id -> Type -- we store the localid of the value within the mod instead
    TTuple :: [Type] -> Type
    TRecord :: (Map.Map RecId Type) -> Type
    deriving (Show, Eq, Ord)

-- Helper functions
-- is this some type of type-class? Functor? but it's non-parametric, makes it a problem, must do manually
mapType :: (Type -> Type) -> Type -> Type
mapType f (TArr t1 t2) = TArr (f t1) (f t2)
mapType f (TTypeCons tName t) = TTypeCons tName (f t)
mapType f (TTuple ts) = TTuple $ map f ts
mapType f (TRecord nTs) = TRecord $ Map.map f nTs
mapType f t = t

mapTypeM :: (Functor m, Applicative m, Monad m) => (Type -> m Type) -> Type -> m Type
mapTypeM f (TArr fromT toT) = TArr <$> f fromT <*> f toT
mapTypeM f (TTypeCons tName t) = TTypeCons tName <$> f t
mapTypeM f (TTuple ts) = TTuple <$> mapM f ts
mapTypeM f (TRecord nTs) = TRecord <$> DT.mapM f nTs
mapTypeM f t = return t

-- utils for converting between tuples and records
dropLabels :: Map.Map String a -> [a]
dropLabels = Map.elems

addLabels :: [a] -> Map.Map String a
addLabels = fst . foldl addLabel (Map.empty, 1)
  where
    addLabel (nXs, i) x = let label = "elem" ++ (show i) in (Map.insert label x nXs, i+1)

-- Bindings ------------------------------------------------------------------------------------------------------------

-- | Bindings within local scope, we use a list for pattern matching on tuples
-- could eventually optimise this and make more type-safe but this works for now
--data Bind b = MultiBind [b] | SingleBind b
--    deriving (Show, Eq, Ord, Functor, DF.Foldable, DT.Traversable)
type BindList a = [a]

-- a variable ref identifier, either local or module scope
data VarId a =  LocalVar a
                | ModVar ModName SrcId
                deriving (Show, Eq, Ord, Functor, DF.Foldable, DT.Traversable)

-- Main Exprs ----------------------------------------------------------------------------------------------------------
-- TODO - could we use the Bind type to unify both b and [b], or use GADTs and type-classes for extra type-safety
-- |Main model elements - maybe move these into a Map indexed by Id
data TopLet b   = TopLet Bool Type (BindList b) (Expr b)   -- binding, expr
--                | TopInitVal Type b (Expr b)
                | TopType b    -- typeid
                deriving (Show, Eq, Ord, Functor, DF.Foldable, DT.Traversable)

-- | Main body of a \c-calc expression
-- is parameterised by the binding type b - used for RdrNames/Ids, Uniques, Uniques+Types
-- is restricted from default \-calc to disallow nested functions, HOF, currying, anony functions and more
-- is extended from default \-calc to support literals (inc. numbers, bools), pairs, and built-in operators (effectily Vars)
-- disabling currying means that all functions take only a single parameter, and evalute to an expression,
-- thus to pass/return multiple values use tuples
-- TODO - should this be a GADT??, should "b" be an instance of Ord
data Expr b = Var (VarId b) (Maybe RecId)             -- a reference to any let-defined expressions
                                        -- could potentially ref to a top-level abs but unlikely, would be optimised

            | App (VarId b) (Expr b)    -- name of top-level func, expression to apply
                                        -- by using an Id instead of Expr w`e effecitively disallow anon-funcs and HOF, we can only
                                        -- call top-level variables that may then be applied

            | Abs b (Expr b)            -- abs arg, expr

            | Let Bool Type (BindList b) (Expr b) (Expr b)  -- basic let within sub-expression
                                                -- test to try multi-lets within an expressino - handles unpacking with context
                                                -- can be stateful bindings that are held between time-steps if 1st param=True
                                                -- Type param holds the type of the bindings created by e1

            | Lit Literal                       -- basic built-in constant literals

            | Op AC.Op (Expr b)         -- is basically identical to App - however is used to refer to built-in/run-time functions
                                        -- we could but don't curry as would like to apply same optimsations to both sys/user functions
                                        -- instead pass pair-cons of expressions

            | If (Expr b) (Expr b) (Expr b)     -- standard FP if construct, used to apply piecewise/case constructs

            | Tuple [Expr b]                    -- a collection of expressions
                                                -- do we allow nested tuples? if not, do we use GADTs to enforce unnested?

            | Record (Map.Map RecId (Expr b))   -- a record, technically just a nmed tuple iwth ordering
                                                -- need to unify with tuples
            -- now add the simulation stuff!
            | Ode (VarId b) (Expr b)            -- an Ode, uses a state variable defined in b,
                                                -- and runs and returns the delta expression

            | Sde (VarId b) (Expr b) (Expr b)   -- a Sde, uses a state variable defined in b, a diffusion coeff in 1st expr,
                                                -- and returns the drift coeff

            | Rre [(Integer, VarId b)] [(Integer, VarId b)] (Expr b) -- an elementary reaction, from var->var with dyn rate expr, returns unit

            | TypeCast (Expr b) (TypeCast b) -- type casts to expressions

            | Group [VarId b]                   -- grouping of state variable references

            deriving (Show, Eq, Ord, Functor, DF.Foldable, DT.Traversable)


-- type casts for expressions
data TypeCast b = UnitCast U.Unit -- a safe cast to the unit for the expr
                | WrapType (VarId b) -- newtype wrapping/unwrapping exprs
                | UnwrapType (VarId b)
                deriving (Show, Eq, Ord, Functor, DF.Foldable, DT.Traversable)


-- | Atomic, core values, will eventually become atomic args during ANF conversion
data Literal =  Num Double U.Unit | NumSeq [Double] U.Unit | Boolean Bool | Time | Wiener | Unit
                deriving (Show, Eq, Ord)

-- Expression Traversal Helper Funcs -----------------------------------------------------------------------------------
-- mapExpr applies a function f over all sub-expressions within the expression
-- is it a functor? doesn't allow chaning type of generic param, as ids then need to be changed too
-- call this only after our own traversals to traverse non-handled cases within f
mapExpr :: (Show a) => (Expr a -> Expr a) -> Expr a -> Expr a
mapExpr f (App v e1) = App v (f e1)
mapExpr f (Abs b e1) = Abs b (f e1)
mapExpr f (Let s t b e1 e2) = Let s t b (f e1) (f e2)
mapExpr f (Op op e1) = Op op (f e1)
mapExpr f (If eB eT eF) = If (f eB) (f eT) (f eF)
mapExpr f (Tuple es) = Tuple (map f es)
mapExpr f (Record es) = Record (Map.map f es)
mapExpr f (Ode v eD) = Ode v (f eD)
mapExpr f (Sde v eW eD) = Sde v (f eW) (f eD)
mapExpr f (Rre srcs dests eR) = Rre srcs dests (f eR)
mapExpr f (TypeCast e1 t) = TypeCast (f e1) t
mapExpr f e = e -- trace' [MkSB e] "Returing unhandled non-composite e" $ e

mapExprM :: (Show a, Applicative m, Monad m) => (Expr a -> m (Expr a)) -> Expr a -> m (Expr a)
mapExprM f (App v e1) = App v <$> f e1
mapExprM f (App v e1) = App v <$> f e1
mapExprM f (Abs b e1) = Abs b <$> f e1
mapExprM f (Let s t b e1 e2) = Let s t b <$> f e1 <*> f e2
mapExprM f (Op op e1) = Op op <$> f e1
mapExprM f (If eB eT eF) = If <$> f eB <*> f eT <*> f eF
mapExprM f (Tuple es) = Tuple <$> mapM f es
mapExprM f (Record es) = Record <$> DT.mapM f es
mapExprM f (Ode v eD) = Ode v <$> f eD
mapExprM f (Sde v eW eD) = Sde v <$> f eW <*> f eD
mapExprM f (Rre srcs dests eR) = Rre srcs dests <$> f eR
mapExprM f (TypeCast e1 t) = TypeCast <$> f e1 <*> pure t
mapExprM f e = return e -- trace' [MkSB e] "Returning unhandled non-composite e" $ return e

-- be carful using these functions, as they handle the continousing fold themeselves
-- we only use these if we need to capture any agg data within the compoosite datatypes
foldExpr :: (Show a, Show b) => (b -> Expr a -> b) -> b -> Expr a -> b
foldExpr f st (App v e1) = f st e1
foldExpr f st (Abs b e1) = f st e1
foldExpr f st (Let s t b e1 e2) = f st e1 |> (\st -> f st e2)
foldExpr f st (Op op e1) = f st e1
foldExpr f st (If eB eT eF) = f st eB |> (\st -> f st eT) |> (\st -> f st eF)
foldExpr f st (Tuple es) = foldl f st es
foldExpr f st (Record es) = Map.foldl f st es
foldExpr f st (Ode v eD) = f st eD
foldExpr f st (Sde v eW eD) = f st eW |> (\st -> f st eD)
foldExpr f st (Rre srcs dest eR) = f st eR
foldExpr f st (TypeCast e1 t) = f st e1
foldExpr f st e = st -- trace' [MkSB e, MkSB st] "Returning unchanged state" st

foldExprM :: (Show a, Show b, Applicative m, Monad m) => (b -> Expr a -> m b) -> b -> Expr a -> m b
foldExprM f st (App v e1) = f st e1
foldExprM f st (Abs b e1) = f st e1
foldExprM f st (Let s t b e1 e2) = f st e1 >>= (\st -> f st e2)
foldExprM f st (Op op e1) = f st e1
foldExprM f st (If eB eT eF) = f st eB >>= (\st -> f st eT) >>= (\st -> f st eF)
foldExprM f st (Tuple es) = DF.foldlM f st es
foldExprM f st (Record es) = DF.foldlM f st es
foldExprM f st (Ode v eD) = f st eD
foldExprM f st (Sde v eW eD) = f st eW >>= (\st -> f st eD)
foldExprM f st (Rre srcs dests eR) = f st eR
foldExprM f st (TypeCast e1 t) = f st e1
foldExprM f st e = return st -- trace' [MkSB e, MkSB st] "Returning unchanged state" $ return st
