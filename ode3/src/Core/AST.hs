-----------------------------------------------------------------------------
--
-- Module      :  Core.AST
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | AST describing the Core language from a model/file perspective, unsure if need to create a
-- | further, functional, lower, ExecutableAST level?
-- | simplified AST - no simulation constuctions, only (named?) components
-- | TODO
-- | * re-add piecewise/case statements
-- | * re-add attribute system
-- | * need to create generic funtor/foldable on AST
-- | * add chemical reactions support
-----------------------------------------------------------------------------

-- export all defs
module Core.AST (
Model, Component(..), CompStmt(..), Expr(..), ExprOp(..)
) where

import Data.Map as Map

type Id = String

-- AST is basically a direct translation of the language syntax/file format

-- top level model
type Model = Map.Map Id Component

-- each independent component, essentially function abstraction
data Component = Component { cName :: Id, cInputs :: [Id], cBody :: [CompStmt], cOutputs :: [Expr]} deriving Show

-- value definitions
data CompStmt   = ValueDef { vName :: [Id], vValue :: Expr }
                | InitValueDef { ivName :: [Id], ivValue :: Expr }
                -- | CompCallDef {  ccOutputs :: [Id], ccName :: Id, ccInputs :: [Expr] }
                | OdeDef { odeName :: Id, odeInit :: Double, odeExp :: Expr}
                deriving Show

-- represents basic arithmetic expressions
data Expr   = BinExpr Expr ExprOp Expr | Number Double
            | FuncCall Id [Expr] | ValueRef Id
            deriving Show

-- basic operators, both binary and unary needed
-- unrary operators - Logical Not, Unary/Numerical Negation
--data NumOp  = Add | Sub | Mul | Div | Mod deriving Show
--data RelOp = LT | LE | GT | GE | EQ | NEQ deriving Show
--data LogOp  = And | Or deriving Show

data ExprOp =   Add | Sub | Mul | Div | Mod
                | LT | LE | GT | GE | EQ | NEQ
                | And | Or deriving Show
