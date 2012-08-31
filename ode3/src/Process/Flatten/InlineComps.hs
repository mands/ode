-----------------------------------------------------------------------------
--
-- Module      :  Process.Flatten.InlineComps
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | This module inlines components within a module
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}

module Process.Flatten.InlineComps (
inlineComps
) where




import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)


import Control.Monad.State

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT

import Utils.CommonImports
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap

import AST.Common as ACO
import qualified AST.Core as AC
import AST.Module
import qualified Subsystem.Units as U

import Subsystem.SysState

-- Unit Conversion -----------------------------------------------------------------------------------------------------

type InlineCompsM = SupplyT Id (StateT InlineState MExcept)

-- holds the current map of (let-bound) abs-expressions within scope
data InlineState = InlineState { absMap :: Map.Map Id (AC.Expr Id), rebindsMap :: Map.Map Id Id, tMap :: TypeMap }
mkInlineState = InlineState Map.empty Map.empty


instance Applicative InlineCompsM where
    pure = return
    (<*>) = ap

inlineComps :: Module Id -> MExcept (Module Id)
inlineComps (LitMod modData) = do
    ((exprMap', freeIds'), _) <- runStateT (runSupplyT inlineCompsM [(modFreeId modData)..]) (mkInlineState $ modTMap modData)
    return $ LitMod $ (updateModData2 modData exprMap') { modFreeId = (head freeIds') }
  where
    inlineCompsM :: InlineCompsM (ExprMap Id)
    inlineCompsM = DT.mapM inlineCompsTop (modExprMap modData)


inlineCompsTop :: AC.TopLet Id -> InlineCompsM (AC.TopLet Id)
-- inlineCompsTop (AC.TopLet isInit bs tE) = AC.TopLet isInit bs <$> AC.mapExprM inlineCconvertTypesExpr tE
inlineCompsTop (AC.TopLet isSval t (b:[]) absE@(AC.Abs arg e)) = do
    -- first inline the abs
    absE' <-  AC.Abs arg <$> inlineCompsExpr e
    -- now store the inlined expr
    lift $ modify (\st -> st { absMap = Map.insert b absE' (absMap st) })
    return (AC.TopLet isSval t (b:[]) absE')

-- now inline any applications
inlineCompsTop (AC.TopLet isSval t (b:[]) appE@(AC.App (AC.LocalVar f) e)) = AC.TopLet isSval t [b] <$> inlineApp f e

-- anything else, keep going
inlineCompsTop e = return e

inlineCompsExpr :: AC.Expr Id -> InlineCompsM (AC.Expr Id)
inlineCompsExpr (AC.Let isSval t (b:[]) absE@(AC.Abs arg e) e2) = do
    -- first inline the abs
    absE' <-  AC.Abs arg <$> inlineCompsExpr e
    -- now store the inlined expr
    lift $ modify (\st -> st { absMap = Map.insert b absE' (absMap st) })
    e2' <- inlineCompsExpr e
    -- remove the inlied expr here
    lift $ modify (\st -> st { absMap = Map.delete b (absMap st) })
    return (AC.Let isSval t (b:[]) absE' e2')

-- now inline any applications
inlineCompsExpr appE@(AC.App (AC.LocalVar f) e) = inlineApp f e
-- anything else, keep going
inlineCompsExpr e = AC.mapExprM inlineCompsExpr e


-- actually setup the AST changes to inline the app of the abs
inlineApp f appE = do
    -- get the abs expr and type
    (AC.Abs arg absE) <- (Map.!) <$> (absMap <$> lift get) <*> pure f
    (AC.TArr _ toT) <- (Map.!) <$> (tMap <$> lift get) <*> pure f
    -- check the rebindsMap is empty
    assert <$> (== 0) <$> (Map.size <$> (rebindsMap <$> lift get)) <*> return ()
    -- first rebind the arg
    arg' <- supply
    lift $ modify (\st -> st { rebindsMap = Map.insert arg arg' (rebindsMap st) })
    -- now rebind the expr
    reboundAbsE <- shiftExprIds absE
    lift $ modify (\st -> st { rebindsMap = Map.delete arg (rebindsMap st) })
    -- finally create a nested let to hold the inlined expr
    -- TODO - what about sVal?
    return $ AC.Let False toT [arg'] appE reboundAbsE


shiftExprIds :: AC.Expr Id -> InlineCompsM (AC.Expr Id)
shiftExprIds (AC.Var (AC.LocalVar v) mRecId) = do
    rbMap <- rebindsMap <$> lift get
    -- if the var has been rebound then replace it
    let v' = maybe v id $ Map.lookup v rbMap
    return $ AC.Var (AC.LocalVar v') mRecId

shiftExprIds (AC.Let isSval t (b:[]) e1 e2) = do
    e1' <- shiftExprIds e1
    -- get a new binding, store and keep going
    b' <- supply
    lift $ modify (\st -> st { rebindsMap = Map.insert b b' (rebindsMap st) })
    e2' <- shiftExprIds e2
    lift $ modify (\st -> st { rebindsMap = Map.delete b (rebindsMap st) })
    return $ AC.Let isSval t [b'] e1' e2'

-- anything else, keep going
shiftExprIds e = AC.mapExprM shiftExprIds e


