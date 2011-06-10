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
Model(..),
ModOpen, ModLocalId(..), Module(..), ModuleAppParams(..), ModuleElem(..),
Component(..), ValueDef(..), CompStmt(..), Expr(..), BinOp(..), UnOp(..),
) where

import Data.Map as Map

type NumTy = Double -- needed?
type Id = String
type ModOpen = String
data ModLocalId = LocalId Id | ModId Id Id deriving Show

-- AST is basically a direct translation of the language syntax/file format

-- top level model
--type Model = Map.Map Id ModuleElem
data Model = Model [ModOpen] [Module] deriving Show

-- module
-- should be split by complete and partial?
data Module = ModuleAbs Id (Maybe [Id]) [ModuleElem]
            | ModuleApp Id ModuleAppParams
            deriving Show

data ModuleAppParams = ModuleAppParams Id (Maybe [ModuleAppParams]) deriving Show

data ModuleElem = ModuleElemComponent Component
                | ModuleElemValue ValueDef
                deriving Show

-- value defintion
-- constant? at least during simgle timestep
data ValueDef = ValueDef { vName :: [Id], vValue :: Expr } deriving Show

-- each independent component, essentially function abstraction
data Component  = Component { cName :: Id, cInputs :: [Id], cBody :: [CompStmt], cOutputs :: [Expr]}
                | ComponentRef Id ModLocalId
                deriving Show


-- value definitions
data CompStmt   = CompValue ValueDef
                | InitValueDef { ivName :: [Id], ivValue :: Expr }
                -- | CompCallDef {  ccOutputs :: [Id], ccName :: Id, ccInputs :: [Expr] }
                | OdeDef { odeName :: Id, odeInit :: Double, odeExp :: Expr}
                | RreDef { rreName :: Id, reaction :: (Id, Id), rate :: Expr}
                deriving Show

-- represents basic arithmetic expressions
data Expr   = BinExpr BinOp Expr Expr | UnExpr UnOp Expr | Number Double | NumSeq Double Double Double
            | Call ModLocalId [Expr] | ValueRef ModLocalId | Piecewise [(Expr, Expr)] Expr
            deriving Show

-- basic operators, both binary and unary needed
-- unary operators - Logical Not, Unary/Numerical Negation
--data NumOp  = Add | Sub | Mul | Div | Mod deriving Show
--data RelOp = LT | LE | GT | GE | EQ | NEQ deriving Show
--data LogOp  = And | Or deriving Show

data BinOp =   Add | Sub | Mul | Div | Mod
                | LT | LE | GT | GE | EQ | NEQ
                | And | Or deriving Show

data UnOp = Not | Neg deriving Show
