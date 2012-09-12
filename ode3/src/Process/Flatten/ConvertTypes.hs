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
-- |
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

import Subsystem.SysState


-- Monad and Helper Funcs -----------------------------------------------------------------------------------------------------

type UnitConvM = SupplyT Id (StateT UnitConvState MExcept)

data UnitConvState = UnitConvState { _inInit :: Bool, _uState :: UnitsState, _tMap :: TypeMap } deriving (Show)
-- type UnitConvState = (UnitsState, TypeMap)
mkUnitConvState = UnitConvState False

modInit :: Bool -> UnitConvM Bool
modInit isInit = do
    oldInit <- lift $ _inInit <$> get
    lift $ modify (\st -> st { _inInit = isInit || (_inInit st) } ) -- set Init flag
    return oldInit

setInit :: Bool -> UnitConvM ()
setInit isInit = lift $ modify (\st -> st { _inInit = isInit } ) -- set Init flag

-- Entry Point ---------------------------------------------------------------------------------------------------------

convertTypes :: Module Id -> UnitsState -> MExcept (Module Id)
convertTypes (LitMod modData) uState = do
    ((exprMap', freeIds'), _) <- runStateT (runSupplyT convTypesM [freeId ..]) $ mkUnitConvState uState (modTMap modData)
    -- update modData and return new module
    let exprMap'' = OrdMap.filter (\topLet -> case topLet of (AC.TopLet _ _ _ _) -> True; (AC.TopType _) -> False) exprMap'
    return $ LitMod $ (updateModData2 modData exprMap'') { modFreeId = (head freeIds') }
  where
    freeId = modFreeId modData
    convTypesM :: UnitConvM (ExprMap Id)
    convTypesM = DT.mapM convertTypesTop (modExprMap modData)


convertTypesTop :: AC.TopLet Id -> UnitConvM (AC.TopLet Id)
convertTypesTop (AC.TopLet isInit t bs tE) = do
    setInit isInit
    AC.TopLet isInit (removeTypeUnits t) bs <$> convertTypesExpr tE
convertTypesTop tLet = return tLet


convertTypesExpr :: AC.Expr Id -> UnitConvM (AC.Expr Id)
convertTypesExpr (AC.Let isInit t bs e1 e2) = do
    oldInit <- modInit isInit
    e1' <- convertTypesExpr e1
    setInit oldInit
    e2' <- convertTypesExpr e2
    return $ AC.Let isInit (removeTypeUnits t) bs e1' e2'

-- drop units from lit nums
convertTypesExpr (AC.Lit (AC.Num n u)) = return $ AC.Lit (AC.Num n U.NoUnit)
-- drop the new-type wraps
convertTypesExpr (AC.TypeCast e (AC.WrapType _)) = convertTypesExpr e
convertTypesExpr (AC.TypeCast e (AC.UnwrapType _)) = convertTypesExpr e
-- convert the unit, create a new let binding with the conversion
convertTypesExpr et@(AC.TypeCast e (AC.UnitCast toU)) = do
    trace' [MkSB et] "in conv unit" $ return ()
    -- first convert the subexpr
    e' <- convertTypesExpr e
    id <- supply
    tMap <- _tMap <$> lift get
    -- get the type of the orig expression
    AC.TFloat fromU <- lift . lift $ T.calcTypeExpr tMap e
    isInit <- _inInit <$> lift get
    AC.Let False (AC.TFloat U.NoUnit) [id] e' <$> (convertUnitCast id fromU toU)

-- don't care about the rest, pass on to mapExprM
convertTypesExpr e = AC.mapExprM convertTypesExpr e

convertUnitCast :: Id -> U.Unit -> U.Unit -> UnitConvM (AC.Expr Id)
convertUnitCast id u1 u2 = do
    -- need to use the unitstate to calc the correct expression and then convert it
    uState <- _uState <$> lift get
    cExpr <- lift . lift $ U.convertCastUnit u1 u2 (L.get lUnitDimEnv uState) (L.get lConvEnv uState)
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

-- takes a type, and removes all units from it
removeTypeUnits :: AC.Type -> AC.Type
removeTypeUnits (AC.TFloat _) = AC.TFloat U.NoUnit
removeTypeUnits t = AC.mapType removeTypeUnits t

