-----------------------------------------------------------------------------
--
-- Module      :  ModelAST
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | ModelAST represents the high level model structure based on
-- | that found in the source files
--
-----------------------------------------------------------------------------

module ModelAST (
    Model(..), NumTy, Expr(..), Component(..), CompStmt(..), BoolExpr(..)
) where

import Data.Map as Map
import AST

-- language statements
data Model = Model {simulates :: [Simulate], components :: Map.Map Id Component} deriving Show

data Component = Component { cName :: Id, cInputs :: [Id], cOutputs :: [Expr],
                             cBody :: [CompStmt] } deriving Show

-- value definitions
data CompStmt =
    ValueDef { vName :: Id, vValue :: Expr }
    | CompCallDef {  ccOutputs :: [Id], ccName :: Id, ccInputs :: [Expr] }
    deriving Show

-- represents basic arithmetic expressions
data Expr   = BinExpr Expr NumOp Expr | Number NumTy
            | FuncCall Id [Expr] | ValueRef Id
            | CaseExp {cases :: [(BoolExpr, Expr)] , def :: Expr}
            | ODE { i :: Id, odeInit :: NumTy, odeExp :: Expr}
            | SDE { i :: Id, sdeInit :: NumTy, sdeExp :: Expr, sdeWein :: Expr}
            deriving Show
            --ExprComponentCall ComponentCall deriving Show

data BoolExpr   = LogExpr BoolExpr LogOp BoolExpr
                | RelExpr Expr RelOp Expr deriving Show

