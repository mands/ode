-----------------------------------------------------------------------------
--
-- Module      :  Core.AST.Expr
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
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
{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}


module Core.ExprAST (
SrcId, Id, VarId(..), Bind(..), Type(..), travTypesM,
TopLet(..), Expr(..), Op(..), Literal(..), UnitT, DesId,
) where

import Control.Monad
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import Data.Functor
import Data.Maybe (fromJust, isJust)
import Utils.Utils
import Common.AST

-- | DetailId - holds both a (parameterised) identifier and a string that represetns the (closest) original/source variable and line num
--data DetailId a = DetailId a SrcId Int deriving (Show, Eq, Ord)

-- | Types
data Type :: * where
    TVar :: Int -> Type
    TBool :: Type
    TFloat :: Type
    TUnit :: Type
    TArr :: Type -> Type -> Type
    TTuple :: [Type] -> Type -- don't want to allow tuples of tuples
    deriving (Show, Eq, Ord)

-- TODO
-- | Unit Dimensions
data Dimensions = Dimensions

-- | Bindings, may be a tuple unpacking
-- could eventually optimise this and make more type-safe but this works for now
-- a unused GADT approach
data TMultiBind
data TSingBind
data TBind :: * -> * -> * where
    TBindM :: [b] -> TBind b TMultiBind
    TBindS :: b -> TBind b TSingBind
    --deriving (Show, Eq, Ord, Functor, DF.Foldable, DT.Traversable)

data Bind b = Bind [b]
    deriving (Show, Eq, Ord, Functor, DF.Foldable, DT.Traversable)

data VarId a =  LocalVar a
                | ModVar SrcId SrcId
                deriving (Show, Eq, Ord, Functor, DF.Foldable, DT.Traversable)

-- TODO - could we use the Bind type to unify both b and [b], or use GADTs and type-classes for extra type-safety
-- |Main model elements - maybe move these into a Map indexed by Id
data TopLet :: * -> * where
    TopLet :: Bool -> (Bind b) -> (Expr b) -> TopLet b    -- binding, expr
    deriving (Show, Eq, Ord, Functor, DF.Foldable, DT.Traversable)

-- | Main body of a \c-calc expression
-- is parameterised by the binding type b - used for RdrNames/Ids, Uniques, Uniques+Types
-- is restricted from default \-calc to disallow nested functions, HOF, currying, anony functions and more
-- is extended from default \-calc to support literals (inc. numbers, bools), pairs, and built-in operators (effectily Vars)
-- disabling currying means that all functions take only a single parameter, and evalute to an expression,
-- thus to pass/return multiple values use tuples
-- TODO - should this be a GADT??, should "b" be an instance of Ord
data Expr b = Var (VarId b)             -- a reference to any let-defined expressions
                                        -- could potentially ref to a top-level abs but unlikely, would be optimised

            | App (VarId b) (Expr b)    -- name of top-level func, expression to apply
                                        -- by using an Id instead of Expr w`e effecitively disallow anon-funcs and HOF, we can only
                                        -- call top-level variables that may then be applied

            | Abs b (Expr b)            -- abs arg, expr

            | Let Bool (Bind b) (Expr b) (Expr b)  -- basic let within sub-expression
                                        -- test to try multi-lets within an expressino - handles unpacking with context
                                         -- can be stateful bindings that are held between time-steps if 1st param=True

            | Lit Literal               -- basic built-in constant literals

            | Op Op (Expr b)    -- is basically identical to App - however is used to refer to built-in/run-time functions
                                -- we could but don't curry as would like to apply same optimsations to both sys/user functions
                                -- instead pass pair-cons of expressions

            | If (Expr b) (Expr b) (Expr b) -- standard if construct, used to apply piecewise/case constructs

            | Tuple [Expr b]            -- a collection of expressions
                                        -- do we allow nested tuples? if not, do we use GADTs to enforce unnested?
            | Ode (VarId b) (Expr b)   -- an Ode, uses a state variable defined in b, and runs the expression,

            | Rre (VarId b) (VarId b) Double -- an RRE, from var->var with given rate

            -- now add the simulation stuff!
            deriving (Show, Eq, Ord, Functor, DF.Foldable, DT.Traversable)

-- | Atomic, core values, will eventually become atomic args during ANF conversion
data Literal =  Num Double | NumSeq [Double] | Boolean Bool | Time | Unit
                deriving (Show, Eq, Ord)

-- | built-in operators - basically any operators that may be expressed directly as hardware instructions or sys/built-ins
data Op = Add | Sub | Mul | Div | Mod
        | LT | LE | GT | GE | EQ | NEQ
        | And | Or | Not
        deriving (Show, Eq, Ord)

-- Helper functions
-- is this some type of type-class? Functor?
travTypesM :: (Monad m) => Type -> (Type -> m Type) -> m Type
travTypesM (TArr fromT toT) f = liftM2 TArr (travTypesM fromT f) (travTypesM toT f)
travTypesM (TTuple ts) f = liftM TTuple $ mapM f ts
travTypesM t f = f t

-- |Standard functor defintion, could be derived automatically but still...
-- only applicable for the binding parameter, so maybe useless
-- could be used to determine bindings/fv, etc.
--instance Functor Bind where
--    fmap f (AbsBind b) = AbsBind $ f b
--    fmap f (LetBind b) = LetBind $ map f b
--
--instance Functor Top where
--    fmap f (TopLet b expr) = TopLet (fmap f b) (fmap f expr)
    --fmap f (TopAbs b arg expr) = TopAbs (fmap f b) (f arg) (fmap f expr)

--instance Functor Expr where
--    fmap f (Var (LocalVar a)) = Var (LocalVar (f a))
--    fmap f (Var (ModVar m a)) = Var (ModVar m a)
--    fmap f (Lit a) = Lit a
--    fmap f (App (LocalVar a) e) = App (LocalVar (f a)) (fmap f e)
--    fmap f (App (ModVar m a) e) = App (ModVar m a) (fmap f e)
--
--    fmap f (Let b e1 e2) = Let (fmap f b) (fmap f e1) (fmap f e2)
--    fmap f (Op op e) = Op op (fmap f e)
--    fmap f (If e1 e2 e3) = If (fmap f e1) (fmap f e2) (fmap f e3)
--    fmap f (Tuple es) = Tuple (List.map (\e -> fmap f e) es)
--
