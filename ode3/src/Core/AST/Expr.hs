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
-- |A lower-level desuagred AST that represents the mian language features
-- is parameterised by the types of values
-- a reference interpreter exists that may execute on the type-checked Core AST
-- bascially the lambda-calculus style IR
-- should be a functor with fmap defined
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies  #-}
{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

module Core.AST.Expr (
SrcId, Id, VarId(..), Bind(..), Type(..), travTypes,
Top(..), Expr(..), Op(..), Literal(..),
) where


import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import Data.Functor
import Data.Maybe (fromJust, isJust)
import Utils.Utils

-- |Basic \-Calc
-- not used - just for reference
-- Positives
-- * Simple, and unified expression and top-level
-- * known basic optimisations, is a known quantity with huge amount of literature
-- * beautiful and expressive
-- Negatives
-- * Allows only single argument for Abstractions, thus result to currying, pair-consing, or tuples for fixed arity funcs
--   (and lists for mult-values, unknown arity expressions)
--   Thus have discord between input params, that are curried, and output params, that would be tuples for mult-vals
--   Currying requires closure creation and optimisation, expensive and complicated while are not expressed in front-end language
--   (tho may make type-cechking easier)
-- * It allows HOF, closures and anonymous functions, the increased flexability/power of these are not required/supported in the front-end
--   and add additional complexity and flexability
-- * It makes it harder to see actual structure of program, need to apply the AST to tell what it's doing,
--   for instance is Apply to a anon func, a local-let func, or a top-level let func?
-- * People modify/extend the \-calc all the time for specific use cases - that is novel in itself and worth it

data LTop       = LTopLet SrcId LExpr

data LExpr      = LVar SrcId
                | LLit Literal
                | LAbs SrcId LExpr
                | LApp LExpr LExpr
                | LLet SrcId LExpr LExpr
                | LPair LExpr LExpr
                | LOp Op
                deriving Show



-- TODO - change to newtype?
-- | Identifier - basicially RdrName - needs to become parameterised
type SrcId = String
type UntypedId = Int
data TypedId = TypedId Int Type
    deriving (Show, Eq, Ord)
type Id = UntypedId
--data ModId a =  LocalId a -- Binding
--                | ModId SrcId a -- Module Name and Binding
--                deriving (Show, Eq, Ord)
--type TestId = ModId SrcId
data VarId a =  LocalVar a
                | ModVar SrcId SrcId
                deriving (Show, Eq, Ord)

-- | DetailId - holds both a (parameterised) identifier and a string that represetns the (closest) original/source variable and line num
-- make use mod id
--data DetailId a = DetailId a SrcId Int deriving (Show, Eq, Ord)
--type Id a = DetailId a

-- | Types
data Type :: * where
    TVar :: Int -> Type
    TBool :: Type
    TFloat :: Type
    TArr :: Type -> Type -> Type
    TTuple :: [Type] -> Type -- don't want to allow tuples of tuples
    deriving (Show, Eq, Ord)

-- could eventually optimise this and make more type-safe but this works for now
data Bind b = AbsBind b | LetBind [b]
    deriving (Show, Eq, Ord)

-- TODO - could we use the Bind type to unify both b and [b], or use GADTs and type-classes for extra type-safety
-- |Main model elements - maybe move these into a Map indexed by Id
data Top b :: * where
    TopLet :: (Bind b) -> (Expr b) -> Top b    -- binding, expr
    TopAbs :: (Bind b) -> b -> (Expr b) -> Top b -- binding, abs name, expr
    deriving (Show, Eq, Ord)

getTopBinding :: Top b -> (Bind b, Top b)
getTopBinding t@(TopLet b _) = (b,t)
getTopBinding t@(TopAbs b _ _) = (b,t)

-- | Main body of a \c-calc expression
-- is parameterised by the binding type b - used for RdrNames/Ids, Uniques, Uniques+Types
-- is restricted from default \-calc to disallow nested functions, HOF, currying, anony functions and more
-- is extended from default \-calc to support literals (inc. numbers, bools), pairs, and built-in operators (effectily Vars)
-- disabling currying means that all functions take only a single parameter, and evalute to an expression,
-- thus to pass/return multiple values simple used pair consing
-- TODO - should this be a GADT??, should "b" be an instance of Ord
data Expr b = Var (VarId b)             -- a reference to any let-defined expressions
                                        -- could potentially ref to a top-level abs but unlikely, would be optimised

            | Lit Literal               -- basic built-in constant literals

            | App b (Expr b)           -- name of top-level func, expression to apply
                                        -- by using an Id instead of Expr we effecitively disallow anon-funcs and HOF, we can only
                                        -- call top-level variables that may then be applied

            | Let (Bind b) (Expr b) (Expr b)  -- basic let within sub-expression
                                        -- test to try multi-lets within an expressino - handles unpacking with context

            | Op Op (Expr b)    -- is basically identical to App - however is used to refer to built-in/run-time functions
                                -- we could but don't curry as would like to apply same optimsations to both sys/user functions
                                -- instead pass pair-cons of expressions

            | If (Expr b) (Expr b) (Expr b) -- standard if construct, used to apply piecewise/case constructs

            | Pair (Expr b) (Expr b)    -- cons expressions into a Pair/Product construct, can nest abritarily to create n-Tuples
                                        -- how do we unpack??
                                        -- can use pattern matching in the front-end/Ode lang, convert it to list of (top-)lets using
                                        -- fst/snd Op functions over the recustive Pair definition
                                        -- how do we force only consing of pairs into n-Tuples?
                                            -- use a list? - but then could have lists-of-lists
                                            -- create own data-type
                                            -- don't worry about it and maybe change later?

            | Tuple [Expr b]            -- just a test, could be used instead of pairs, makes some ops easier

            -- now add the simulation stuff!
            deriving (Show, Eq, Ord)

-- |Atomic, core values, will eventually become atomic args during ANF conversion
data Literal =  Num Double | NumSeq [Double] | Boolean Bool
                deriving (Show, Eq, Ord)

-- |built-in operators - basically any operators that may be expressed directly as hardware instructions or sys/built-ins
data Op = Add | Sub | Mul | Div | Mod
        | LT | LE | GT | GE | EQ | NEQ
        | And | Or | Not
        deriving (Show, Eq, Ord)

-- is this some type of type-class?
travTypes :: Type -> (Type -> Type) -> Type
travTypes (TArr fromT toT) f = TArr (travTypes fromT f) (travTypes toT f)
travTypes (TTuple ts) f = TTuple $ map f ts
travTypes t f = f t

-- |Standard functor defintion, could be derived automatically but still...
-- only applicable for the binding parameter, so maybe useless
-- could be used to determine bindings/fv, etc.

instance Functor Bind where
    fmap f (AbsBind b) = AbsBind $ f b
    fmap f (LetBind b) = LetBind $ map f b

instance Functor Top where
    fmap f (TopLet b expr) = TopLet (fmap f b) (fmap f expr)
    fmap f (TopAbs b arg expr) = TopAbs (fmap f b) (f arg) (fmap f expr)

instance Functor Expr where
    fmap f (Var (LocalVar a)) = Var (LocalVar (f a))
    fmap f (Var (ModVar m a)) = Var (ModVar m a)
    fmap f (Lit a) = Lit a
    fmap f (App x e) = App (f x) (fmap f e)
    fmap f (Let b e1 e2) = Let (fmap f b) (fmap f e1) (fmap f e2)
    fmap f (Op op e) = Op op (fmap f e)
    fmap f (If e1 e2 e3) = If (fmap f e1) (fmap f e2) (fmap f e3)
    fmap f (Pair e1 e2) = Pair (fmap f e1) (fmap f e2)
    fmap f (Tuple es) = Tuple (List.map (\e -> fmap f e) es)


instance PrettyPrint Op where
    prettyPrint Add = "+"
    prettyPrint Sub = "-"
    prettyPrint Mul = "*"
    prettyPrint Div = "/"
    prettyPrint Mod = "%"

    prettyPrint Core.AST.Expr.LT = "<"
    prettyPrint LE = "<="
    prettyPrint Core.AST.Expr.GT = ">"
    prettyPrint GE = ">="
    prettyPrint Core.AST.Expr.EQ = "=="
    prettyPrint NEQ = "!="

    prettyPrint And = "&&"
    prettyPrint Or = "!!"
    prettyPrint Not = "!"

instance PrettyPrint Literal where
    prettyPrint (Num a) = show a
    prettyPrint (NumSeq a) = show a
    prettyPrint (Boolean a) = show a

instance PrettyPrint SrcId where
    prettyPrint id = show id

instance (Show a) => PrettyPrint (Top a) where
    prettyPrint (TopLet x expr) = "let " ++ show x ++ " = " ++ prettyPrint expr
    prettyPrint (TopAbs x arg expr) = show x ++ " = \\" ++ show arg ++ ". " ++ prettyPrint expr

instance (Show a) => PrettyPrint (Expr a) where
    prettyPrint (Var x) = show x
    prettyPrint (Lit x) = case x of
                            Num y -> show y
                            NumSeq ys -> show ys
                            Boolean b -> show b

    prettyPrint (App x expr) = show x ++ prettyPrint expr

    prettyPrint (Let x bindExpr inExpr) = "let " ++ show x ++ " = " ++ prettyPrint bindExpr ++ " in \n" ++ prettyPrint inExpr

    prettyPrint (Op op expr) = prettyPrint op ++ prettyPrint expr
    prettyPrint (If ifExpr tExpr fExpr) = "if (" ++ prettyPrint ifExpr ++ ") then " ++ prettyPrint tExpr ++ " else " ++
                                            prettyPrint fExpr

    prettyPrint (Pair e1 e2) = "(" ++ prettyPrint e1 ++ ", " ++ prettyPrint e2 ++ ")"

    prettyPrint (Tuple es) = "(" ++ tuples ++ ")"
      where
        tuples = concat . Prelude.map (\e -> (prettyPrint e) ++ ", ") $ es


