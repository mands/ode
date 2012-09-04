-----------------------------------------------------------------------------
--
-- Module      :  Lang.CoreFlat.AST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | TODO
--
-----------------------------------------------------------------------------

module AST.CoreFlat (
Module(..), ExprMap, TopLet, Expr(..), Var(..), ExprData(..), Type(..)
) where

-- import Data.IntMap as IM
import qualified Utils.OrdMap as OrdMap
import AST.Common as AC

-- not really a module, but datatype to hold both the exeutable simulation expressions and related metadata
data Module = Module { loopExprs :: ExprMap,  initExprs :: ExprMap, freeId :: Id }
            deriving (Show, Eq, Ord)

-- this becomes our 'let' now - both toplevel and 'nested', creates a new binding for the expression
-- ordering is maintained as ids are ascending -- TODO, check??
-- is essentiallaly a list of variable bindings within a program
type ExprMap = OrdMap.OrdMap TopLet ExprData

-- we can change this later to handle both single and multibind variables
-- (need for Ops returning multiple vals)
type TopLet = Id

data ExprData = ExprData Expr Type deriving (Show, Eq, Ord)

-- our minimal type info, held at every let/var binding
data Type = TFloat | TBool | TUnit | TTuple [Type] deriving (Show, Eq, Ord)

-- the main type of our simulatable expressions - basically ANF form
data Expr   = Var Var
            | Op AC.Op [Var]
            | If Var ExprMap ExprMap    -- nested envs for each conditional branch
            | Tuple [Var]               -- needed for MRVs, nested tuples not allowed
            | Ode Id Var
            -- Rre Id Id Expr           -- add later to separate exprs
            deriving (Show, Eq, Ord)

-- | Atomic, core values
data Var = VarRef Id | TupleRef Id Integer | Num Double | Boolean Bool | Unit
                deriving (Show, Eq, Ord)


-- | Used to handle both input and output args to a library/runtime operator
-- these are handled in an implementation-specific fashion for the given backend
-- data OpParams = OpParams [Id] deriving (Show, Eq, Ord)

-- Traversal Functions -------------------------------------------------------------------------------------------------
-- TODO
