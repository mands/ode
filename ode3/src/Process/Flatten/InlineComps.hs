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
-- NOTE - this does handle recursion at all
-- - should either throw an erro on recursive component, or only expand upto a limit (aka C++ templates)
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

-- State Monads --------------------------------------------------------------------------------------------------------

type InlineCompsM = SupplyT Id (StateT InlineState MExcept)

-- holds the current map of (let-bound) abs-expressions within scope
data InlineState = InlineState { absMap :: Map.Map Id (AC.Expr Id), rebindsMap :: Map.Map Id Id, tMap :: TypeMap }
mkInlineState = InlineState Map.empty Map.empty

-- Entry Point ---------------------------------------------------------------------------------------------------------
inlineComps :: Module Id -> MExcept (Module Id)
inlineComps (LitMod modData) = do
    ((exprMap', freeIds'), _) <- runStateT (runSupplyT inlineCompsM [(modFreeId modData)..]) (mkInlineState $ modTMap modData)
    return $ LitMod $ (updateModData2 modData exprMap') { modFreeId = (head freeIds') }
  where
    inlineCompsM :: InlineCompsM (ExprMap Id)
    inlineCompsM = do
        -- inline components, drop top abs
        exprMap' <- DT.mapM inlineCompsTop (modExprMap modData)
        return $ OrdMap.filter filterTopAbs exprMap'

    filterTopAbs :: AC.TopLet Id -> Bool
    filterTopAbs (AC.TopLet _ _ _ absE@(AC.Abs _ _)) = False
    filterTopAbs tl = True


inlineCompsTop :: AC.TopLet Id -> InlineCompsM (AC.TopLet Id)
inlineCompsTop (AC.TopLet isInit t (b:[]) absE@(AC.Abs arg e)) = do
    -- first inline the abs
    absE' <-  AC.Abs arg <$> inlineCompsExpr e
    -- now store the inlined expr
    lift $ modify (\st -> st { absMap = Map.insert b absE' (absMap st) })
    return $ AC.TopLet isInit t (b:[]) absE'

-- inline any applications
inlineCompsTop (AC.TopLet isInit t bs e) = AC.TopLet isInit t bs <$> inlineCompsExpr e

-- anything else, i.e TopType, keep going
inlineCompsTop e = return e


inlineCompsExpr :: AC.Expr Id -> InlineCompsM (AC.Expr Id)
inlineCompsExpr abs@(AC.Let isInit t (b:[]) absE@(AC.Abs arg e) e2) = do
    -- first inline the abs
    absE' <-  AC.Abs arg <$> inlineCompsExpr e
    -- now store the inlined expr
    lift $ modify (\st -> st { absMap = Map.insert b absE' (absMap st) })
    e2' <- inlineCompsExpr e2
    -- remove the inlied expr here
    lift $ modify (\st -> st { absMap = Map.delete b (absMap st) })
    -- drop the abs (as we've already saved it), and return the rest of the let expr
    return e2' -- (AC.Let isInit t (b:[]) absE' e2')

inlineCompsExpr (AC.Let isInit t bs e1 e2) = AC.Let isInit t bs <$> inlineCompsExpr e1 <*> inlineCompsExpr e2

-- now inline any applications
inlineCompsExpr appE@(AC.App (AC.LocalVar f) e) = inlineApp f e
-- anything else, keep going
inlineCompsExpr e = AC.mapExprM inlineCompsExpr e

-- | actually setup the AST changes to inline the app of the abs
-- replace an app abs e -> let
inlineApp f appE = do
    -- get the abs expr and type
    abs@(AC.Abs arg absE) <- (Map.!) <$> (absMap <$> lift get) <*> pure f
    (AC.TArr fromT toT) <- (Map.!) <$> (tMap <$> lift get) <*> pure f
    -- check the rebindsMap is empty
    assert <$> (== 0) <$> (Map.size <$> (rebindsMap <$> lift get)) <*> return ()
    -- first rebind the arg
    arg' <- supply
    lift $ modify (\st -> st { rebindsMap = Map.insert arg arg' (rebindsMap st) })
    -- now rebind the expr
    reboundAbsE <- shiftExprIds absE
    lift $ modify (\st -> st { rebindsMap = Map.delete arg (rebindsMap st) })
    -- finally create a nested let to hold the inlined expr
    return $ AC.Let False fromT [arg'] appE reboundAbsE

-- | shift all variables within a sub-expression, creating new tmp vars to hold lets and rebinding any refs
shiftExprIds :: AC.Expr Id -> InlineCompsM (AC.Expr Id)
shiftExprIds (AC.Var v mRecId) = AC.Var <$> calcReboundVar v <*> pure mRecId

shiftExprIds (AC.Ode v eD) = AC.Ode <$> calcReboundVar v <*> shiftExprIds eD

shiftExprIds (AC.Sde v eW eD) = AC.Sde <$> calcReboundVar v <*> shiftExprIds eW <*> shiftExprIds eD

shiftExprIds (AC.Rre vs1 vs2 rate) = AC.Rre <$> shiftRres vs1 <*> shiftRres vs2 <*> pure rate
  where
    shiftRres = mapM (mapSndM calcReboundVar)


shiftExprIds (AC.Let isInit t (b:[]) e1 e2) = do
    e1' <- shiftExprIds e1
    -- get a new binding, store and keep going
    b' <- supply
    lift $ modify (\st -> st { rebindsMap = Map.insert b b' (rebindsMap st) })
    e2' <- shiftExprIds e2
    lift $ modify (\st -> st { rebindsMap = Map.delete b (rebindsMap st) })
    return $ AC.Let isInit t [b'] e1' e2'

-- anything else, keep going
shiftExprIds e = AC.mapExprM shiftExprIds e

calcReboundVar :: AC.VarId Id -> InlineCompsM (AC.VarId Id)
calcReboundVar (AC.LocalVar v) = do
    rbMap <- rebindsMap <$> lift get
    -- if the var has been rebound then replace it
    let v' = maybe v id $ Map.lookup v rbMap
    return $ AC.LocalVar v'
