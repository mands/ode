-----------------------------------------------------------------------------
--
-- Module      :  Process.Flatten.ConvertTypes
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Removes type defs, converts unit expressions into std expressions and removes units annotations, removes newtype wraps
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}

module Process.Flatten.ConvertTypes (
convertTypes
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
import qualified Subsystem.Types as T

import qualified Subsystem.SysState as S


-- Monad and Helper Funcs -----------------------------------------------------------------------------------------------------

type UnitConvM = SupplyT Id (StateT UnitConvState MExcept)

data UnitConvState = UnitConvState { _uState :: S.UnitsState, _tMap :: TypeMap, _timeUnit :: U.Unit } deriving (Show)
-- type UnitConvState = (UnitsState, TypeMap)
mkUnitConvState = UnitConvState

-- Entry Point ---------------------------------------------------------------------------------------------------------

convertTypes :: S.UnitsState -> U.Unit -> Module Id -> MExcept (Module Id)
convertTypes uState tUnit (LitMod modData) = do

    trace' [MkSB modData] "Initial Types" $ return ()

    ((exprMap', freeIds'), _) <- runStateT (runSupplyT convTypesM [freeId ..]) $ mkUnitConvState uState (modTMap modData) tUnit
    -- drop the toptypes from AST
    let exprMap'' = OrdMap.filter (\topLet -> case topLet of (AC.TopLet _ _ _ _) -> True; (AC.TopType _) -> False) exprMap'
    -- update modData and return new module

    trace' [MkSB exprMap''] "Converted Types" $ return ()

    return $ LitMod $ (updateModData2 modData exprMap'') { modFreeId = (head freeIds') }
  where
    freeId = modFreeId modData
    convTypesM :: UnitConvM (ExprMap Id)
    convTypesM = DT.mapM convertTypesTop (modExprMap modData)


convertTypesTop :: AC.TopLet Id -> UnitConvM (AC.TopLet Id)
convertTypesTop (AC.TopLet isInit t bs tE) = do
    AC.TopLet isInit (removeTypeCasts t) bs <$> convertTypesExpr tE
convertTypesTop tLet = return tLet


convertTypesExpr :: AC.Expr Id -> UnitConvM (AC.Expr Id)
convertTypesExpr (AC.Let isInit t bs e1 e2) = do
    AC.Let isInit (removeTypeCasts t) bs <$> convertTypesExpr e1 <*> convertTypesExpr e2

-- drop units from lit nums
convertTypesExpr (AC.Lit (AC.Num n u)) = return $ AC.Lit (AC.Num n U.NoUnit)
-- drop the new-type wraps
convertTypesExpr (AC.TypeCast e (AC.WrapType _)) = convertTypesExpr e
convertTypesExpr (AC.TypeCast e (AC.UnwrapType _)) = convertTypesExpr e
-- convert the unit, create a new let binding with the conversion
convertTypesExpr et@(AC.TypeCast e (AC.UnitCast toU)) = do
    -- trace' [MkSB et] "in conv unit" $ return ()
    -- first convert the subexpr
    e' <- convertTypesExpr e
    id <- supply
    tMap <- _tMap <$> lift get
    tUnit <- _timeUnit <$> lift get

    -- get the type of the orig expression
    AC.TFloat fromU <- lift . lift $ T.calcTypeExpr (tMap, tUnit) e
    AC.Let False (AC.TFloat U.NoUnit) [id] e' <$> (convertUnitCast id fromU toU)

-- don't care about the rest, pass on to mapExprM
convertTypesExpr e = AC.mapExprM convertTypesExpr e

convertUnitCast :: Id -> U.Unit -> U.Unit -> UnitConvM (AC.Expr Id)
convertUnitCast id u1 u2 = do
    -- need to use the unitstate to calc the correct expression and then convert it
    uState <- _uState <$> lift get
    cExpr <- lift . lift $ U.convertCastUnit u1 u2 (L.get S.lUnitDimEnv uState) (L.get S.lConvEnv uState)
    return $ convertUnitExpr id cExpr

-- converts an expression from the restrictred CExpr format into the general Core Expression for code-gen
convertUnitExpr :: Id -> U.CExpr -> AC.Expr Id
convertUnitExpr id (U.CExpr op e1 e2) = AC.Op (convertUnitOp op) $ AC.Tuple [convertUnitExpr id e1, convertUnitExpr id e2]
  where
    convertUnitOp U.CAdd = AC.BasicOp Add
    convertUnitOp U.CSub = AC.BasicOp Sub
    convertUnitOp U.CMul = AC.BasicOp Mul
    convertUnitOp U.CDiv = AC.BasicOp Div

convertUnitExpr id (U.CNum n) = AC.Lit $ AC.Num n U.NoUnit
convertUnitExpr id U.CFromId = AC.Var (AC.LocalVar id) Nothing

-- takes a type, and removes all units/newtype info from it
removeTypeCasts :: AC.Type -> AC.Type
removeTypeCasts (AC.TFloat _) = AC.TFloat U.NoUnit
-- removeTypeCasts (AC.TWrap _ _) = AC.TUnit
removeTypeCasts t = AC.mapType removeTypeCasts t

