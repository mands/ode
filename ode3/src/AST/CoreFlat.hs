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
Module(..), ExprMap, TopLet, Expr(..), Var(..)
) where

-- import Data.IntMap as IM
import qualified Utils.OrdMap as OrdMap
import AST.Common as AC

-- not really a module, but datatype to hold both the exeutable simulation expressions and related metadata
data Module = Module { loopExprs :: ExprMap,  initExprs :: ExprMap, freeId :: Id }
            deriving (Show, Eq, Ord)

-- this becomes our 'let' now - both toplevel and 'nested', creates a new binding for the expression
-- ordering is maintained as ids are ascending -- TODO, check??
type ExprMap = OrdMap.OrdMap TopLet Expr

-- we can change this later to handle both single and multibind variables
-- (need for Ops returning multiple vals)
type TopLet = Id


-- the main type of our simulatable expressions - basically ANF form
data Expr   = Var Var
            | Op AC.Op [Var]
            | If Var ExprMap ExprMap
            -- Tuple [Expr]          -- do we have tuples? NO, unpack all
            -- Record (Map.Map RecId Expr) -- no records, unpack all
            | Ode Id Var
            -- Rre Id Id Expr     -- add later
            deriving (Show, Eq, Ord)

-- | Atomic, core values
data Var = VarRef Id | Num Double | Boolean Bool | Unit
                deriving (Show, Eq, Ord)

-- | Used to handle both input and output args to a library/runtime operator
-- these are handled in an implementation-specific fashion for the given backend
-- data OpParams = OpParams [Id] deriving (Show, Eq, Ord)
