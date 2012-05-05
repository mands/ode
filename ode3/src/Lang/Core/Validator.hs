-----------------------------------------------------------------------------
--
-- Module      :  Core.Validator
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | A basic validator for a Module expressions, perfromas very simple validation tasks
-- hopefully GADTs will remove a lot of these
--
-----------------------------------------------------------------------------

module Lang.Core.Validator (
validate,
) where

import Control.Monad.Error
import Control.Applicative
import Data.Functor
import Data.List (nub)
import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import qualified Data.Set as Set
import Utils.Utils
import qualified Utils.OrdMap as OrdMap
import qualified Lang.Core.AST as E
import qualified Lang.Module.AST as M

validate :: M.Module E.DesId -> MExcept (M.Module E.DesId)
validate mod@(M.LitMod _ modData) = M.LitMod <$> createTopExprs (M.modExprList modData) <*> pure (modData { M.modExprList = [] })
validate mod@(M.FunctorMod funArgs _ modData) = M.FunctorMod <$> funArgs' <*> createTopExprs (M.modExprList modData)
                                                <*> pure (modData { M.modExprList = [] })
  where
    funArgs' =  if (length funArgKeys == (length . nub) funArgKeys) then pure funArgs
                else throwError ("(VL05) - Functor has arguments with the same name")
    funArgKeys = OrdMap.keys funArgs
validate mod@(M.AppMod _ _) = pure mod

-- create the expression map and check for duplicated top-level bindings
createTopExprs :: M.ExprList -> MExcept (M.ExprMap E.DesId)
createTopExprs exprList = snd <$> DF.foldlM t (Set.empty, OrdMap.empty) exprList
  where
    t :: (Set.Set E.SrcId, M.ExprMap E.DesId) -> E.TopLet E.DesId -> MExcept (Set.Set E.SrcId, M.ExprMap E.DesId)
    t s topExpr@(E.TopLet sv b expr) = validExpr (Set.empty) expr *> addTopBinding s b topExpr

    addTopBinding (topBinds, exprMap) b expr = case b of
        -- E.SingBind ab -> (,) <$> addBinding topBinds ab <*> pure (OrdMap.insert b expr exprMap)
        E.Bind bs -> (,) <$> DF.foldlM addBinding topBinds bs <*> pure (OrdMap.insert b expr exprMap)
      where
        addBinding topBinds (b, _) = case Set.member b topBinds of
            True -> throwError $ "(VL06) - Top Binding " ++ (show b) ++ " already exists in module"
            False -> pure $ (Set.insert b topBinds)

-- check several properties for expression tree, passes state down into exp, doesn't bother returning it for now
validExpr :: (Set.Set E.SrcId) -> E.Expr E.DesId -> MExcept ()
-- validExpr _ e@(E.Var v) = pure ()
-- validExpr _ e@(E.Lit l) = pure ()

validExpr curBinds (E.App v e) = validExpr curBinds e

validExpr curBinds (E.Abs b e) = validExpr curBinds e

validExpr curBinds (E.Let s (E.Bind bs) e1 e2) = do
    curBinds' <- DF.foldlM addBinding curBinds bs
    (validExpr Set.empty e1) *> (validExpr curBinds' e2)
  where
    addBinding curBinds (b, _) = case Set.member b curBinds of
        True -> throwError $ "(VL04) - Binding " ++ (show b) ++ " already exists at this scoping level"
        False -> pure $ (Set.insert b curBinds)

validExpr curBinds (E.Op op e) = validExpr curBinds e
validExpr curBinds (E.If eB eT eF) = (validExpr curBinds eB) *> (validExpr curBinds eT) *> (validExpr curBinds eF)

validExpr curBinds (E.Tuple []) = throwError "(VL01) Empty tuple found"
validExpr curBinds (E.Tuple (e:[])) = throwError "(VL02) Tuple with only one element found"
validExpr curBinds (E.Tuple es) = DF.traverse_ (validExpr curBinds) es

validExpr _ e = pure ()


-- TraveExpr applies a function f over all sub-expressions within the expression
-- is it a functor?
-- travExpr :: (E.Expr E.SrcId -> b) -> E.Expr E.SrcId -> b
travExpr f e@(E.Var v) = f e
travExpr f e@(E.Lit l) = f e
travExpr f (E.App v e1) = f (E.App v (travExpr f e1))
travExpr f (E.Let s b e1 e2) = f (E.Let s b (travExpr f e1) (travExpr f e2))
travExpr f (E.Op op e) = f (E.Op op (travExpr f e))
travExpr f (E.If eB eT eF) = f (E.If (travExpr f eB) (travExpr f eT) (travExpr f eF))
travExpr f (E.Tuple es) = f $ E.Tuple (map (travExpr f) es)
travExpr f e = f e

