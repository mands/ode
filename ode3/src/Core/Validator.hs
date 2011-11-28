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

module Core.Validator (
validate,
) where

import Control.Monad.Error
import Control.Applicative
import Data.Functor
import Data.List (nub)
import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import qualified Data.Set as Set

import Core.ExprAST as E
import Core.ModuleAST as M
import Utils.Utils
import qualified Utils.OrdMap as OrdMap

validate :: M.Module E.SrcId -> MExcept (M.Module E.SrcId)
validate mod@(M.LitMod exprMap modData) = M.LitMod <$> validTopExpr exprMap <*> pure modData
validate mod@(M.FunctorMod funArgs exprMap modData) = M.FunctorMod <$> funArgs' <*> validTopExpr exprMap <*> pure modData
  where
    funArgs' = if (length funArgKeys == (length . nub) funArgKeys) then pure funArgs else throwError ("(VL05) - Functor has arguments with the same name")
    funArgKeys = OrdMap.keys funArgs
validate mod@(M.AppMod _ _) = pure mod

-- check for duplicated top-level bindings here
validTopExpr :: M.ExprMap E.SrcId -> MExcept (M.ExprMap E.SrcId)
validTopExpr exprMap = (\_ -> exprMap) <$> DF.foldlM t Set.empty exprMap
  where
    t :: (Set.Set E.SrcId) -> E.Top E.SrcId -> MExcept (Set.Set E.SrcId)
    t topBinds (E.TopAbs b arg expr) = validExpr (Set.singleton arg) expr *> addTopBinding topBinds b
    t topBinds (E.TopLet b expr) = validExpr (Set.empty) expr *> addTopBinding topBinds b

    addTopBinding topBinds b = case b of
        E.AbsBind ab -> addBinding topBinds ab
        E.LetBind bs -> DF.foldlM addBinding topBinds bs
      where
        addBinding topBinds b = case Set.member b topBinds of
            True -> throwError $ "(VL05) - Top Binding " ++ (show b) ++ " already exists in module"
            False -> pure $ (Set.insert b topBinds)

-- check several properties for expression tree, passes state down into exp, doesn't bother returning it for now
validExpr :: (Set.Set E.SrcId) -> E.Expr E.SrcId -> MExcept ()
validExpr bs e@(E.Var v) = pure ()
validExpr bs e@(E.Lit l) = pure ()
validExpr curBinds (E.App v e1) = validExpr curBinds e1 *> pure ()

validExpr curBinds (E.Let b'@(E.LetBind bs) e1 e2) = do
    curBinds' <- DF.foldlM addBinding curBinds bs
    --E.Let <$> pure b' <*> (validExpr curBinds' e1) <*> (validExpr curBinds' e2)
    (validExpr curBinds e1) *> (validExpr curBinds' e2) *> pure ()
  where
    addBinding curBinds b = case Set.member b curBinds of
        True -> throwError $ "(VL04) - Binding " ++ (show b) ++ " already exists in component"
        False -> pure $ (Set.insert b curBinds)

validExpr curBinds (E.Op op e) = (validExpr curBinds e) *> pure () --E.Op <$> pure op <*> (validExpr curBinds e)
validExpr curBinds (E.If eB eT eF) =
    (validExpr curBinds eB) *> (validExpr curBinds eT) *> (validExpr curBinds eF) *> pure ()
validExpr _ (E.Tuple []) = throwError "(VL01) Empty tuple found"
validExpr _ (E.Tuple (e:[])) = throwError "(VL02) Tuple with only one element found"
validExpr curBinds (E.Tuple es) = DF.traverse_ (validExpr curBinds) es
validExpr _ e = pure ()


--travExpr :: (E.Expr E.SrcId -> b) -> E.Expr E.SrcId -> b
travExpr f e@(E.Var v) = f e
travExpr f e@(E.Lit l) = f e
travExpr f (E.App v e1) = f (E.App v (travExpr f e1))
travExpr f (E.Let b e1 e2) = f (E.Let b (travExpr f e1) (travExpr f e2))
travExpr f (E.Op op e) = f (E.Op op (travExpr f e))
travExpr f (E.If eB eT eF) = f (E.If (travExpr f eB) (travExpr f eT) (travExpr f eF))
travExpr f (E.Tuple es) = f $ E.Tuple (map (travExpr f) es)
travExpr f e = f e



