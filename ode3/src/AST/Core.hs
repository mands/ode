-----------------------------------------------------------------------------
--
-- Module      :  Core.Expr.AST
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
{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures, DataKinds #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}


module AST.Core (
VarId(..), BindList,
Type(..), mapTypeM, mapType, addLabels, dropLabels,
TopLet(..), Expr(..), Op(..), Literal(..), TypeCast(..),
SrcId, DesId, Id, RecId -- rexported from Common.AST
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
    TVar :: Int -> Type
    TBool :: Type
    TFloat :: U.Unit -> Type
    TUnit :: Type
    -- Composite Types
    TArr :: Type -> Type -> Type
    -- We use ModFullName as a common ID between TTypeCons and TWrap in order to check they types are valid
    -- NOTE - don't need to add SrcId as ID too, as duplicate bindings not allowed within module
    TTypeCons :: ModFullName -> Type -> Type
    TWrap :: ModFullName -> SrcId -> Type
    TTuple :: [Type] -> Type
    TRecord :: (Map.Map RecId Type) -> Type
    deriving (Show, Eq, Ord)

-- Helper functions
-- is this some type of type-class? Functor? but it's non-parametric, makes it a problem, must do manually
mapType :: (Type -> Type) -> Type -> Type
mapType f (TArr t1 t2) = TArr (mapType f t1) (mapType f t2)
mapType f (TTypeCons tName t) = TTypeCons tName (mapType f t)
mapType f (TTuple ts) = TTuple $ map (mapType f) ts
mapType f (TRecord nTs) = TRecord $ Map.map (mapType f) nTs
mapType f t = f t

mapTypeM :: (Monad m) => (Type -> m Type) -> Type -> m Type
mapTypeM f (TArr fromT toT) = liftM2 TArr (mapTypeM f fromT) (mapTypeM f toT)
mapTypeM f (TTypeCons tName t) = liftM (TTypeCons tName) (mapTypeM f t)
mapTypeM f (TTuple ts) = liftM TTuple $ mapM (mapTypeM f) ts
mapTypeM f (TRecord nTs) = liftM TRecord $ DT.mapM (mapTypeM f) nTs
mapTypeM f t = f t

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
data TopLet :: * -> * where
    TopLet :: Bool -> (BindList b) -> (Expr b) -> TopLet b    -- binding, expr
    TopType :: b -> TopLet b    -- typeid
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

            | Let Bool (BindList b) (Expr b) (Expr b)  -- basic let within sub-expression
                                        -- test to try multi-lets within an expressino - handles unpacking with context
                                         -- can be stateful bindings that are held between time-steps if 1st param=True

            | Lit Literal               -- basic built-in constant literals

            | Op AC.Op (Expr b)    -- is basically identical to App - however is used to refer to built-in/run-time functions
                                -- we could but don't curry as would like to apply same optimsations to both sys/user functions
                                -- instead pass pair-cons of expressions

            | If (Expr b) (Expr b) (Expr b) -- standard if construct, used to apply piecewise/case constructs

            | Tuple [Expr b]            -- a collection of expressions
                                        -- do we allow nested tuples? if not, do we use GADTs to enforce unnested?

            | Record (Map.Map RecId (Expr b)) -- a record, technically just a nmed tuple iwth ordering - need to unify with tuples

            | Ode (VarId b) (Expr b)   -- an Ode, uses a state variable defined in b, and runs and returns the delta expression

            | Rre (VarId b) (VarId b) Double -- an RRE, from var->var with given rate

            | TypeCast (Expr b) (TypeCast b) -- type casts to expressions
            -- now add the simulation stuff!
            deriving (Show, Eq, Ord, Functor, DF.Foldable, DT.Traversable)


-- type casts for expressions
data TypeCast b = UnitCast U.Unit -- a safe cast to the unit for the expr
                | WrapType (VarId b) -- newtype wrapping/unwrapping exprs
                | UnwrapType (VarId b)
                deriving (Show, Eq, Ord, Functor, DF.Foldable, DT.Traversable)


-- | Atomic, core values, will eventually become atomic args during ANF conversion
data Literal =  Num Double U.Unit | NumSeq [Double] U.Unit | Boolean Bool | Time | Unit
                deriving (Show, Eq, Ord)


-- mapExpr applies a function f over all sub-expressions within the expression
-- is it a functor?
mapExpr :: (Show a) => (Expr a -> Expr a) -> Expr a -> Expr a
mapExpr f (App v e1) = f (App v (mapExpr f e1))
mapExpr f (Abs b e1) = f (Abs b (mapExpr f e1))
mapExpr f (Let s b e1 e2) = f (Let s b (mapExpr f e1) (mapExpr f e2))
mapExpr f (Op op e) = f (Op op (mapExpr f e))
mapExpr f (If eB eT eF) = f (If (mapExpr f eB) (mapExpr f eT) (mapExpr f eF))
mapExpr f (Tuple es) = f $ Tuple (map (mapExpr f) es)
mapExpr f (Record es) = f $ Record (Map.map (mapExpr f) es)
mapExpr f (Ode v e1) = f (Ode v (mapExpr f e1))
mapExpr f (TypeCast e1 t) = f (TypeCast (mapExpr f e1) t)
mapExpr f e = trace' [MkSB e] "Applying mapExprM to base e" $ f e

mapExprM :: (Show a, Applicative m, Monad m) => (Expr a -> m (Expr a)) -> Expr a -> m (Expr a)
mapExprM f (App v e1) = f =<< App v <$> mapExprM f e1
mapExprM f (Abs b e1) = f =<< Abs b <$> mapExprM f e1
mapExprM f (Let s b e1 e2) = f =<< Let s b <$> mapExprM f e1 <*> mapExprM f e2
mapExprM f (Op op e) = f =<< Op op <$> mapExprM f e
mapExprM f (If eB eT eF) = f =<< If <$> mapExprM f eB <*> mapExprM f eT <*> mapExprM f eF
mapExprM f (Tuple es) = f =<< Tuple <$> mapM (mapExprM f) es
mapExprM f (Record es) = f =<< Record <$> DT.mapM (mapExprM f) es
mapExprM f (Ode v e1) = f =<< Ode v <$> mapExprM f e1
mapExprM f (TypeCast e1 t) = f =<< TypeCast <$> mapExprM f e1 <*> pure t
mapExprM f e = trace' [MkSB e] "Applying mapExprM to base e" $ f e