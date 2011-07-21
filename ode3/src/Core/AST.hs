-----------------------------------------------------------------------------
--
-- Module      :  Core.AST
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
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-} --, FlexibleInstances #-}

module Core.AST (
Model, Id, Top(..), Expr(..), Op(..), Literal(..)
) where

import Data.Map as Map
import Utils.Utils

-- |Identifier - basicially RdrName - needs to become parameterised
type Id = String
-- |NewIdentifier - holds both a (parameterised) identifier and a string that represetns the (closest) original/source variable
data NewId a = NewId a String


-- |Top level Core model
-- need to make sure this is an ordered map so we keep the evaluation order correct
-- maybe use number and Id to index/key
type Model b =  Map Id (Top b)

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

data LTop       = LTopLet Id LExpr

data LExpr      = LVar Id
                | LLit Literal
                | LAbs Id LExpr
                | LApp LExpr LExpr
                | LLet Id LExpr LExpr
                | LPair LExpr LExpr
                | LOp Op
                deriving Show

-- |Main model elements - maybe move these into a Map indexed by Id
data Top b  = TopLet b (Expr b)    -- binding, expr
            | TopAbs b b (Expr b) -- binding, abs name, expr
            deriving Show

-- | Main body of a \c-calc expression
-- is parameterised by the binding type b - used for RdrNames/Ids, Uniques, Uniques+Types
-- is restricted from default \-calc to disallow nested functions, HOF, currying, anony functions and more
-- is extended from default \-calc to support literals (inc. numbers, bools), pairs, and built-in operators (effectily Vars)
-- disabling currying means that all functions take only a single parameter, and evalute to an expression,
-- thus to pass/return multiple values simple used pair consing
data Expr b = Var b                    -- a reference to any let-defined expressions
                                        -- could potentially ref to a top-level abs but unlikely, would be optimised

            | Lit Literal               -- basic built-in constant literals

            | App b (Expr b)           -- name of top-level func, expression to apply
                                        -- by using an Id instead of Expr we effecitively disallow anon-funcs and HOF, we can only
                                        -- call top-level variables that may then be applied

            | Let b (Expr b) (Expr b)  -- basic let within sub-expression

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
            deriving Show

-- |Atomic, core values, will eventually become atomic args during ANF conversion
data Literal =  Num Double | NumSeq [Double] | Boolean Bool
                deriving Show

-- |built-in operators - basically any operators that may be expressed directly as hardware instructions or sys/built-ins
data Op = Add | Sub | Mul | Div | Mod
        | LT | LE | GT | GE | EQ | NEQ
        | And | Or | Not
        | Fst | Snd | Unpack Int -- used for unpacking values from Pairs/Tuples
        | Nop -- not used?
        deriving Show





-- create a few typeclasss instances

-- |Standard functor defintion, could be derived automatically but still...
-- only applicable for the binding parameter, so maybe useless
-- could be used to determine bindings/fv, etc.
instance Functor Expr where
    fmap f (Var a) = Var (f a)
    fmap f (Lit a) = Lit a
    fmap f (App x e) = App (f x) (fmap f e)
    fmap f (Let x e1 e2) = Let (f x) (fmap f e1) (fmap f e2)
    fmap f (Op op e) = Op op (fmap f e)
    fmap f (If e1 e2 e3) = If (fmap f e1) (fmap f e2) (fmap f e3)
    fmap f (Pair e1 e2) = Pair (fmap f e1) (fmap f e2)
    fmap f (Tuple es) = Tuple (Prelude.map (\e -> fmap f e) es)

instance PrettyPrint Op where
    prettyPrint Add = "+"
    prettyPrint Sub = "-"
    prettyPrint Mul = "*"
    prettyPrint Div = "/"
    prettyPrint Mod = "%"

    prettyPrint Core.AST.LT = "<"
    prettyPrint LE = "<="
    prettyPrint Core.AST.GT = ">"
    prettyPrint GE = ">="
    prettyPrint Core.AST.EQ = "=="
    prettyPrint NEQ = "!="

    prettyPrint And = "&&"
    prettyPrint Or = "!!"
    prettyPrint Not = "!"

    prettyPrint Fst = ".1"
    prettyPrint Snd = ".2"
    prettyPrint (Unpack a) = "!!" ++ show a

    prettyPrint Nop = "NOP"

instance PrettyPrint Literal where
    prettyPrint (Num a) = show a
    prettyPrint (NumSeq a) = show a
    prettyPrint (Boolean a) = show a

instance PrettyPrint Id where
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

--type Model b =  Map Id (Top b)
instance (Show a) => PrettyPrint (Model a) where
    prettyPrint model = unlines . Prelude.map (\e -> prettyPrint e) $ Map.elems model



