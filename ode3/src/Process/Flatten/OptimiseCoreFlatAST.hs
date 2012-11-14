-----------------------------------------------------------------------------
--
-- Module      :  Process.Flatten.OptimiseCoreFlatAST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Performs some simple simulation-specific optimisations on the CoreFlat-level AST by pattern-matching
--
-- currently includes
-- * Power function expansion
--
-- NOTE - THIS MODULE IS CURRENTLY UNUSED
-----------------------------------------------------------------------------

module Process.Flatten.OptimiseCoreFlatAST (
optimiseCoreFlatAST
) where

import Control.Monad.State
import Utils.CommonImports

import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Traversable as DT
import qualified Utils.OrdMap as OrdMap

import Subsystem.SysState
import Utils.MonadSupply

import AST.Common as AC
import AST.CoreFlat

-- Types ---------------------------------------------------------------------------------------------------------------
-- no (state) monad needed yet

-- Process Entry -------------------------------------------------------------------------------------------------------
optimiseCoreFlatAST :: Module -> MExcept Module
optimiseCoreFlatAST Module{..} = do
    let initExprs' = runOps initExprs
    let loopExprs' = runOps loopExprs
    return $ Module loopExprs' initExprs' simOps freeId
  where
    runOps exprMap = fmap optPowerTop exprMap

-- Perform Power Expansion ---------------------------------------------------------------------------------------------

optPowerTop :: ExprData -> ExprData
optPowerTop (ExprData e t) = ExprData (optPowerExpr e) t

optPowerExpr :: Expr -> Expr
-- pow(x, +n) => expand to n multiplications of x
optPowerExpr e@(Op (AC.MathOp (AC.Pow)) (v1:(Num n):[])) = trace' [MkSB e, MkSB e'] "Pow Expansion" $ e'
  where
    e' = e


-- don't care about the rest, pass on to mapExprM
optPowerExpr e = mapExpr optPowerTop e

