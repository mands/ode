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
-- | AST describing the Ode language from a model/file perspective,
-- is then converted to a further, functional, lower, Core level
-- simplified AST - no simulation constuctions, only (named?) components
-- AST is basically a direct translation of the language syntax/file format
-----------------------------------------------------------------------------

module AST.Ode (
    RefId(..), BindId(..), OdeStmt(..), Stmt(..),
    Expr(..), -- BinOp(..), UnOp(..),
    SrcId, NumTy -- rexported from Common.AST
) where

import qualified Data.Map as Map
import AST.Common

-- | SrcIdentifier that may be reference a local or module parameter
data RefId = LocalId SrcId | ModId SrcId SrcId deriving (Show, Eq, Ord)

-- | used for creating new bindings that may differntiatin between actual ids and _ vals
data BindId = BindId SrcId | DontCare deriving (Show, Ord, Eq)

data OdeStmt =  ExprStmt Stmt
                | ImportStmt ModImport
                -- a quantity defition, in terms of a dim vector
                | QuantityStmt { qName :: SrcId, qDim :: DimVec }
                -- a unit defntions, in terms of a unit sequence for a particular dimension
                | UnitStmt { uName :: BaseUnit, uDim :: Maybe Char, uAlias :: Maybe String, uSI :: Bool}
                | ConvDefStmt { cFrom :: BaseUnit, cTo :: BaseUnit, cExpr :: CExpr }
                | TypeStmt { tType :: SrcId }
                deriving (Show, Eq, Ord)

-- | elements allowed within a module, basically components or top-level constant values
data Stmt = -- each independent component, is basically function abstraction
            -- components may be defined inline, with name, ins, outs, and body
            Component { cName :: SrcId, cArg :: [BindId], cBody :: ([Stmt], Expr)}
            -- value defintion
            -- they are constant, at least during single timestep
            | Value { vName :: [BindId], vValue :: ([Stmt], Expr) }
            -- state value defintion - indirectly mutable, stateful, values - only single bind name
            | SValue { svName :: SrcId, svValue :: Expr }
            -- ODE - a SValue and ODE def combined
            | OdeDef { odeInit :: RefId, odeDeltaName :: BindId, odeDeltaExpr :: Expr}
            -- SDE - a SValue and SDE def combined
            | SdeDef { sdeInit :: RefId, sdeDeltaName :: BindId, sdeDiffusionExpr :: Expr, sdeDriftExpr :: Expr}
            -- RRE - takes two SValues and a rate parameter
            | RreDef { rreSrc :: [(Integer, RefId)], rreDest :: [(Integer, RefId)], rreRate :: Expr }
            -- or they may be a reference to a component defined in a module param and re-exported here
            -- ComponentRef SrcId ModLocalId
            deriving (Show, Eq, Ord)

-- | tree for basic arithmetic expressions, these are recursive and may include
-- binary and unary opertors, literal numbers, number sequences ([a,b..c]),
-- calls to local/module components and run-time functions
-- refernces to existing values, piecewise terms
-- expressions are may be multiple types, these are determined later on
data Expr   =   Op Op [Expr] | Number Double (Maybe UnitList) | NumSeq Double Double Double | Boolean Bool
                | Time | Wiener | None | Call RefId [Expr] | ValueRef RefId (Maybe SrcId) | Piecewise [(Expr, Expr)] Expr
                | Tuple [Expr] | Record [(SrcId, Expr)]
                -- type/unit commands
                | ConvCast Expr UnitList | WrapType Expr RefId | UnwrapType Expr RefId
                deriving (Show, Eq, Ord)
