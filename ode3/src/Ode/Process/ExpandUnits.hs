-----------------------------------------------------------------------------
--
-- Module      :  Process.ExpandUnits
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Ode.Process.ExpandUnits (
expandUnits,
) where

import Control.Monad.Error
import Control.Applicative
import Data.Functor
import Data.List (nub)
import qualified Data.Map as Map

import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import qualified Data.Set as Set
import Text.Printf (printf)
import Ode.Utils.CommonImports
import qualified Ode.Utils.OrdMap as OrdMap
import qualified Ode.AST.Core as E
import qualified Ode.AST.Module as M
import AST.Common as AC
import qualified Ode.Subsystem.SysState as St
import qualified Ode.Subsystem.Units as U


-- Process Entry -------------------------------------------------------------------------------------------------------
expandUnits :: St.UnitsState -> M.Module E.DesId -> MExcept (M.Module E.DesId)
expandUnits St.UnitsState{_derivedEnv} mod@(M.LitMod modData) = M.LitMod <$> expandUnits' _derivedEnv modData
expandUnits St.UnitsState{_derivedEnv} mod@(M.FunctorMod funArgs modData) = M.FunctorMod funArgs <$> expandUnits' _derivedEnv modData


-- Derived unit expansion --------------------------------------------------------------------------
-- HACK - last-min pass to expand derived unit into base units

-- gen the map, and
expandUnits' :: U.DerivedUnitEnv -> (M.ModData E.DesId) -> MExcept (M.ModData E.DesId)
expandUnits' dEnv modData@M.ModData{modExprMap, modDerived} = do
    let exprMap' = fmap dUnitExpandTop modExprMap
    return $ modData { M.modExprMap = exprMap' }
  where
    -- combine main eEnv and quantities defn in current mod
    jointDEnv = U.addDerivedUnitsToEnv dEnv modDerived

    dUnitExpandTop :: E.TopLet E.DesId -> E.TopLet E.DesId
    dUnitExpandTop (E.TopLet isInit t bs tE) = E.TopLet isInit t bs $ dUnitExpandExpr tE
    dUnitExpandTop t = t

    -- need to fold over ul - checking against unitstate and cur mod data
    dUnitExpandExpr :: E.Expr E.DesId -> E.Expr E.DesId
    dUnitExpandExpr e@(E.Lit (E.Num n (U.DerivedUnit ul))) = e' -- trace' [MkSB jointDEnv, MkSB e, MkSB e'] "Derived unit expansion" $ e'
      where
        e' = E.Lit (E.Num n $ expandUnit ul)

    dUnitExpandExpr e@(E.TypeCast e1 (E.UnitCast (U.DerivedUnit ul))) = e' -- trace' [MkSB jointDEnv, MkSB e, MkSB e'] "Derived unit expansion" $ e'
      where
        e' = E.TypeCast (dUnitExpandExpr e1) (E.UnitCast $ expandUnit ul)

    dUnitExpandExpr e = E.mapExpr dUnitExpandExpr e

    expandUnit :: U.UnitList -> U.Unit
    expandUnit ul = U.mkUnit . reverse $ foldl expandUnit' [] ul
      where
        expandUnit' :: U.UnitList -> (BaseUnit, Integer) -> U.UnitList
        expandUnit' ul (uName, uNum) = case Map.lookup uName jointDEnv of
            Nothing -> (uName, uNum) : ul
            Just baseUl | (U.UnitC baseUl') <- U.mulUnit (U.mkUnit baseUl) uNum -> baseUl' ++ ul
            Just _ ->  (uName, uNum) : ul

