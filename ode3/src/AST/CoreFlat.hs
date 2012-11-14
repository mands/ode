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
Module(..), ExprMap, TopLet, Expr(..), Var(..), ExprData(..), Type(..), SimOps(..),
mapExpr
) where

-- import Data.IntMap as IM
import qualified Utils.OrdMap as OrdMap
import AST.Common as AC

-- not really a module, but datatype to hold both the exeutable simulation expressions and related metadata
data Module = Module    { loopExprs :: ExprMap,  initExprs :: ExprMap, simOps :: [SimOps]
                        , freeId :: Id
                        }
            deriving (Show, Eq, Ord)

-- this becomes our 'let' now - both toplevel and 'nested', creates a new binding for the expression
-- ordering is maintained as ids are ascending -- TODO, check??
-- is essentiallaly a list of const-val bindings within a program
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
            | If Var ExprMap ExprMap    -- nested envs for each conditional branch (i.e. BasicBlocks)
            deriving (Show, Eq, Ord)

-- | Atomic, core values
data Var    = VarRef Id
            | TupleRef Id Integer
            | Tuple [Var]               -- needed from MRVs, should never be nested
            | Num Double | Boolean Bool | Unit | Time
            deriving (Show, Eq, Ord)

-- | Main simulation opersions
data SimOps = Ode Id Var    -- indicates the state val and a ref to an id holding the delta val
                            -- TODO - convert to an Id instead of Var
            -- TODO
            --Sde Id Var
            --Rre Id Id Var
            deriving (Show, Eq, Ord)

-- | Used to handle both input and output args to a library/runtime operator
-- these are handled in an implementation-specific fashion for the given backend
-- data OpParams = OpParams [Id] deriving (Show, Eq, Ord)

-- Traversal Functions -------------------------------------------------------------------------------------------------

mapExpr :: (ExprData -> ExprData) -> Expr -> Expr
mapExpr f (If v eT eF) = If v (fmap f eT) (fmap f eF)
mapExpr f e = e -- trace' [MkSB e] "Returing unhandled non-composite e" $ e
