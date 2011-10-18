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
--
-----------------------------------------------------------------------------

module Core.Validator (
validate,
) where

import Control.Monad.Error
import Control.Applicative
import Data.Functor
import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import qualified Data.Set as Set

import Core.AST as C
import Utils.Utils
import qualified Utils.OrdMap as OrdMap

validate :: C.Module C.SrcId -> MExcept (C.Module C.SrcId)
validate mod@(C.LitMod exprMap modData) = C.LitMod <$> validTopExpr exprMap <*> pure modData
validate mod@(C.FunctorMod funArgs exprMap modData) = C.FunctorMod <$> pure funArgs <*> validTopExpr exprMap <*> pure modData
validate mod@(C.AppMod _ _) = pure mod

validTopExpr :: C.ExprMap C.SrcId -> MExcept (C.ExprMap C.SrcId)
validTopExpr exprMap = (\_ -> exprMap) <$> DF.traverse_ t exprMap
  where
    t (C.TopAbs b arg expr) = validExpr (Set.singleton arg) expr
    t (C.TopLet b expr) = validExpr (Set.empty) expr

-- check several properties for expression tree, passes state down into exp, doesn't bother returning it for now
validExpr :: (Set.Set C.SrcId) -> C.Expr C.SrcId -> MExcept ()
validExpr bs e@(C.Var v) = pure ()
validExpr bs e@(C.Lit l) = pure ()
validExpr curBinds (C.App v e1) = validExpr curBinds e1 *> pure ()

validExpr curBinds (C.Let b'@(C.LetBind bs) e1 e2) = do
    curBinds' <- DF.foldlM addBinding curBinds bs
    --C.Let <$> pure b' <*> (validExpr curBinds' e1) <*> (validExpr curBinds' e2)
    (validExpr curBinds e1) *> (validExpr curBinds' e2) *> pure ()
  where
    addBinding curBinds b = case Set.member b curBinds of
        True -> throwError $ "(VL04) - Binding " ++ (show b) ++ " already exists in component"
        False -> pure $ (Set.insert b curBinds)

validExpr curBinds (C.Op op e) = (validExpr curBinds e) *> pure () --C.Op <$> pure op <*> (validExpr curBinds e)
validExpr curBinds (C.If eB eT eF) =
    (validExpr curBinds eB) *> (validExpr curBinds eT) *> (validExpr curBinds eF) *> pure ()
validExpr _ (C.Tuple []) = throwError "(VL01) Empty tuple found"
validExpr _ (C.Tuple (e:[])) = throwError "(VL02) Tuple with only one element found"
validExpr curBinds (C.Tuple es) = DF.traverse_ (validExpr curBinds) es
validExpr _ e = pure ()


--travExpr :: (C.Expr C.SrcId -> b) -> C.Expr C.SrcId -> b
travExpr f e@(C.Var v) = f e
travExpr f e@(C.Lit l) = f e
travExpr f (C.App v e1) = f (C.App v (travExpr f e1))
travExpr f (C.Let b e1 e2) = f (C.Let b (travExpr f e1) (travExpr f e2))
travExpr f (C.Op op e) = f (C.Op op (travExpr f e))
travExpr f (C.If eB eT eF) = f (C.If (travExpr f eB) (travExpr f eT) (travExpr f eF))
travExpr f (C.Tuple es) = f $ C.Tuple (map (travExpr f) es)
travExpr f e = f e



