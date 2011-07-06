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

module Core.AST (
Model, Id, Top(..), Expr(..), Op(..), Literal(..)
) where

import Data.Map as Map

-- |Identifier - basicially RdrName - needs to become parameterised
type Id = String

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

            | Tuple [Expr b]            -- NOT IMPLEMENTED...
                                        -- just a test, could be used instead of pairs, makes some ops easier

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
