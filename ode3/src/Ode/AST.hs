-----------------------------------------------------------------------------
--
-- Module      :  Ode.AST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |AST describing the Ode language from a model/file perspective,
-- is then converted to a further, functional, lower, Core level
-- simplified AST - no simulation constuctions, only (named?) components
-- AST is basically a direct translation of the language syntax/file format
-- TODO
-- * re-add piecewise/case statements
-- * re-add attribute system
-- * need to create generic funtor/foldable on AST
-- * add chemical reactions support
-----------------------------------------------------------------------------

module Ode.AST (
    ModLocalId(..), TopElem(..),
    Component(..), ValueDef(..), CompStmt(..), Expr(..), BinOp(..), UnOp(..),
    SrcId, NumTy
) where

import Data.Map as Map
import Common.AST

-- | an individual number type, not sure if needed, used to convert from double to integer
--type NumTy = Double
-- | identifier - is converted later on
--type Id = String
-- | SrcIdentifier that may be local to current block or refer to a module parameter
data ModLocalId = LocalId SrcId | ModId SrcId SrcId
                deriving Show


-- |elements allowed within a module, basically components or top-level constant values
data TopElem = TopElemComponent Component
                | TopElemValue ValueDef
                deriving Show

-- |value defintion
-- they are constant, at least during single timestep
data ValueDef = ValueDef { vName :: [SrcId], vValue :: Expr } deriving Show

-- |each independent component, it is essentially a function abstraction
-- components may be defined inline, with name, ins, outs, and body
-- or they may be a reference to a component defined in a module param and re-exported here
data Component  = Component { cName :: SrcId, cInputs :: [SrcId], cOutputs :: [Expr], cBody :: [CompStmt]}
                | ComponentRef SrcId ModLocalId
                deriving Show

-- |statments allowed within a component, these include,
-- constant value defintinos, intial value defintions, odes and rres
data CompStmt   = CompValue ValueDef
                | InitValueDef { ivName :: [SrcId], ivValue :: Expr }
                | OdeDef { odeName :: SrcId, odeInit :: Double, odeExp :: Expr}
                | RreDef { rreName :: SrcId, reaction :: (SrcId, SrcId), rate :: Expr}
                -- CompCallDef {  ccOutputs :: [SrcId], ccName :: SrcId, ccInputs :: [Expr] }
                deriving Show

-- |tree for basic arithmetic expressions, these are recursive and may include
-- binarry and unary opertors, literal numbers, number sequences ([a,b..c]),
-- calls to local/module components and run-time functions
-- refernces to existing values, piecewise terms
-- expressions are may be multiple types, these are determined later on
data Expr   = BinExpr BinOp Expr Expr | UnExpr UnOp Expr | Number Double | NumSeq Double Double Double | Boolean Bool
            | Call ModLocalId [Expr] | ValueRef ModLocalId | Piecewise [(Expr, Expr)] Expr
            deriving Show

-- |basic binary expression operators
data BinOp  = Add | Sub | Mul | Div | Mod
            | LT | LE | GT | GE | EQ | NEQ
            | And | Or
            deriving Show

-- |basic unary expression operators
data UnOp   = Not | Neg
            deriving Show
