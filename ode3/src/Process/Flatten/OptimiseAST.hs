-----------------------------------------------------------------------------
--
-- Module      :  Process.Flatten.OptimiseAST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Performs some simple - simulation-speifici optimisations on the Core-level AST
-- currently this only includes short-circuiting of boolean operators
-- others to follow
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}

module Process.Flatten.OptimiseAST (
optimiseCoreAST
) where

import Control.Monad.State
import Utils.CommonImports

import AST.Common as ACO
import qualified AST.Core as AC
import AST.Module

optimiseCoreAST :: Module Id -> Bool -> MExcept (Module Id)
optimiseCoreAST (LitMod modData) shortCircuit = do
    let exprMap' = if shortCircuit then optSCETop <$> modExprMap modData else modExprMap modData

    return $ LitMod $ modData { modExprMap = exprMap' } -- updateModData2 modData exprMap'


-- Perform Short-Circuit Evaluation ------------------------------------------------------------------------------------

optSCETop :: AC.TopLet Id -> AC.TopLet Id
optSCETop (AC.TopLet isInit t bs tE) = AC.TopLet isInit t bs $ optSCEExpr tE

optSCEExpr :: AC.Expr Id -> AC.Expr Id
-- e1 AND e2 => if (e1) then e2 else False
optSCEExpr e@(AC.Op (ACO.BasicOp (ACO.And)) (AC.Tuple (e1:e2:[]))) = trace' [MkSB e, MkSB e'] "SC AND Expr" $ e'
  where
    e' = AC.If e1 e2 (AC.Lit $ AC.Boolean False)

-- e1 OR e2 => if (e1) then True else (e2)
optSCEExpr e@(AC.Op (ACO.BasicOp (ACO.Or)) (AC.Tuple (e1:e2:[]))) = trace' [MkSB e, MkSB e'] "SC OR Expr" $ e'
  where
    e' = AC.If e1 (AC.Lit $ AC.Boolean True) e2
-- don't care about the rest, pass on to mapExprM
optSCEExpr e = AC.mapExpr optSCEExpr e

-- Other Opts - TODO ---------------------------------------------------------------------------------------------------
