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

module Lang.Ode.AST (
    ModLocalId(..), ValId(..), OdeStmt(..), Stmt(..),
    Expr(..), BinOp(..), UnOp(..),
    SrcId, NumTy -- rexported from Common.AST
) where

import qualified Data.Map as Map
import Lang.Common.AST

-- | SrcIdentifier that may be local to current block or refer to a module parameter
data ModLocalId =   LocalId SrcId | ModId SrcId SrcId deriving (Show, Eq, Ord)

-- | used for creating new bindings that may differntiatin between actual ids and _ vals
data ValId = ValId SrcId | DontCare deriving (Show, Ord, Eq)

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
            Component { cName :: SrcId, cInputs :: [ValId], cOutputs :: Expr, cBody :: [Stmt]}
            -- value defintion
            -- they are constant, at least during single timestep
            | Value { vName :: [ValId], vValue :: Expr, vBody :: [Stmt] }
            -- state value defintion - indirectly mutable, stateful, values
            | SValue { svName :: [ValId], svValue :: [Double] }
            -- ODE - a SValue and ODE def combined
            | OdeDef { odeName :: ValId, odeInit :: Double, odeExpr :: Expr}
            -- RRE - takes two SValues and a rate parameter
            | RreDef { rreRate :: Double, rreSrc :: SrcId, rreDest :: SrcId }
            -- or they may be a reference to a component defined in a module param and re-exported here
            -- ComponentRef SrcId ModLocalId
            deriving (Show, Eq, Ord)


-- | statments allowed within a component, these include,
-- constant value defintinos, intial value defintions, odes and rres
--data CompStmt   = CompValue Value       -- nested value
--                | CompComp Component    -- nested component
--                | InitValueDef { ivName :: [SrcId], ivValue :: Expr }
--                | OdeDef { odeName :: SrcId, odeInit :: Double, odeExp :: Expr}
--                | RreDef { rreName :: SrcId, reaction :: (SrcId, SrcId), rate :: Expr}
--                deriving Show

-- | tree for basic arithmetic expressions, these are recursive and may include
-- binary and unary opertors, literal numbers, number sequences ([a,b..c]),
-- calls to local/module components and run-time functions
-- refernces to existing values, piecewise terms
-- expressions are may be multiple types, these are determined later on
data Expr   = BinExpr BinOp Expr Expr | UnExpr UnOp Expr | Number Double (Maybe UnitList) | NumSeq Double Double Double | Boolean Bool
                    | Time | Unit | Call ModLocalId [Expr] | ValueRef ModLocalId (Maybe SrcId) | Piecewise [(Expr, Expr)] Expr
                    | Tuple [Expr] | Record [(SrcId, Expr)]
                    | ConvCast Expr UnitList
                    | WrapType SrcId Expr | UnwrapType SrcId Expr
                    deriving (Show, Eq, Ord)

-- | basic binary expression operators
data BinOp  = Add | Sub | Mul | Div | Mod
            | LT | LE | GT | GE | EQ | NEQ
            | And | Or
            deriving (Show, Eq, Ord)

-- | basic unary expression operators
data UnOp   = Not | Neg
            deriving (Show, Eq, Ord)
