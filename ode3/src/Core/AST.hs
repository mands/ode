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
Model
) where

import Data.Map as Map


-- |Identifier - basicially RdrName - needs to become parameterised
type Id = String


-- |Top level Core model
type Model =  [Int]

-- |Main model elements - maybe move these into a Map
data TopLevel   = TopVal Id Expr
                | TopFunc Id Func

-- |Main \-calc function abstraction
data Func   = Abs Id Func
            | Expr Expr
--            |  App Id Id
--            | Let Id Expr Func -- lets can only be to expressions, not function defintinos

-- | Main body of a \-calc expression
-- is separated in order to disallow nested functions, HOF, and more
data Expr   = Let Id Expr Expr
            | App Id [Expr]
            | Op Op [Expr]
            | Number [Double] -- numbers may be single or a list of possibles for sens analysis
            | If Expr Expr Expr
            | MultVal [Expr]
            -- now add the simulation stuff
            deriving Show

-- |built-in operators - basically any operators that may be expressed directly as hardware instructions
data Op = Add | Sub | Mul | Div | Mod
        | LT | LE | GT | GE | EQ | NEQ
        | And | Or | Not
        deriving Show
