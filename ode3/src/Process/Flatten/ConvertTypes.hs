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
convertTypes, calcTypeExpr
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

type UnitConvM = SupplyT Id (StateT UnitConvState MExcept)

instance Applicative UnitConvM where
    pure = return
    (<*>) = ap

-- data UnitConvState = UnitConvState { uState :: UnitsState, typeMap :: TypeMap } deriving (Show, Eq, Ord)
type UnitConvState = (UnitsState, TypeMap)

convertTypes :: Module Id -> UnitsState -> MExcept (Module Id)
convertTypes (LitMod modData) uState = do
    ((exprMap', freeIds'), (_, tMap')) <- runStateT (runSupplyT convTypesM [freeId ..]) (uState, (modTMap modData))
    let (tMap'', exprMap'') = dropTCons tMap' exprMap' modData

    -- update modData and return new module
    let modData' = modData { modExprMap = exprMap'', modFreeId = (head freeIds'), modTMap = tMap'' }
    return $ LitMod modData'
  where
    freeId = modFreeId modData
    convTypesM :: UnitConvM (ExprMap Id)
    convTypesM = DT.mapM convertTypesTop (modExprMap modData)


convertTypesTop :: AC.TopLet Id -> UnitConvM (AC.TopLet Id)
convertTypesTop (AC.TopLet isInit t bs tE) = AC.TopLet isInit t bs <$> convertTypesExpr tE
convertTypesTop tLet = return tLet

convertTypesExpr :: AC.Expr Id -> UnitConvM (AC.Expr Id)
-- drop units from lit nums
convertTypesExpr (AC.Lit (AC.Num n u)) = return $ AC.Lit (AC.Num n U.NoUnit)
-- drop the new-type wraps
convertTypesExpr (AC.TypeCast e (AC.WrapType _)) = return e
convertTypesExpr (AC.TypeCast e (AC.UnwrapType _)) = return e
-- convert the unit, create a new let binding with the conversion
convertTypesExpr (AC.TypeCast e (AC.UnitCast toU)) = do
    id <- supply
    (_, tMap) <- lift get
    AC.TFloat fromU <- lift . lift $ calcTypeExpr tMap e
    -- TODO - what about the SVal type??
    AC.Let False (AC.TFloat U.NoUnit) [id] e <$> (convertUnitCast id fromU toU)

-- don't care about the rest, pass on to mapExprM
convertTypesExpr e = AC.mapExprM convertTypesExpr e

convertUnitCast :: Id -> U.Unit -> U.Unit -> UnitConvM (AC.Expr Id)
convertUnitCast id u1 u2 = do
    -- need to use the unitstate to calc the correct expression and then convert it
    (uState, _) <- lift get
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


-- update typemap, first map over types, then drop TopType&TTypeCons
dropTCons tMap exprMap modData = (tMap', exprMap')
  where
    exprMap' = OrdMap.filter (\topLet -> case topLet of (AC.TopLet _ _ _ _) -> True; (AC.TopType _) -> False) exprMap
    tMap' = Map.map (AC.mapType updateType) tMap |> Map.filter (\t -> case t of (AC.TTypeCons _ _) -> False; _ -> True)

    -- drop type info
    updateType = undefined
--    updateType (AC.TFloat u) = AC.TFloat U.NoUnit
--    -- have to use IdBimap to resovle tName into its actual TTypeCons
--    updateType (AC.TWrap modName tName) = t
--      where
--        (AC.TTypeCons _ t) = tMap Map.! ((modIdBimap modData) Bimap.! tName)
--    updateType t = t

-- Type Helper Funcs -----------------------------------------------------------------------------------------------------
-- TODO - should this be moved??

-- calculates the type of any abritary expression via recusrive descent and tMap lookup
-- assumes both that type-checking hsa completed (hence valid typemap is present) and modules are inlined/not presesent
-- we need this as we only store types on at (top-)let points
calcTypeExpr :: TypeMap -> AC.Expr Id -> MExcept AC.Type
calcTypeExpr tMap (AC.Var (AC.LocalVar v) Nothing) = return $ tMap Map.! v
calcTypeExpr tMap (AC.Var (AC.LocalVar v) (Just recId)) | (AC.TRecord ts) <- tMap Map.! v = return $ ts Map.! recId

calcTypeExpr tMap (AC.App (AC.LocalVar f) _) | AC.TArr eT toT <- (tMap Map.! f) = return toT

-- what is the type of the arg? AC.TArr Unit (calcTypeExpr tMap e)
-- calcTypeExpr tMap (AC.Abs arg e) = undefined -- calcTypeExpr tMap e

calcTypeExpr tMap (AC.Let s t bs e1 e2) = calcTypeExpr tMap e2

calcTypeExpr _ (AC.Lit l) = case l of
    AC.Boolean _ -> return AC.TBool
    AC.Num _ u -> return $ AC.TFloat u
    AC.Time -> return $ AC.TFloat U.uSeconds
    AC.Unit -> return AC.TUnit

calcTypeExpr tMap (AC.If eB eT eF) = calcTypeExpr tMap eT

-- are these unpacked yet?
calcTypeExpr tMap (AC.Tuple es) = AC.TTuple <$> DT.mapM (calcTypeExpr tMap) es
-- is a literal record, record this within the type
calcTypeExpr tMap (AC.Record nEs) = AC.TRecord <$> DT.mapM (calcTypeExpr tMap) nEs

calcTypeExpr tMap (AC.Ode lv@(AC.LocalVar _) eD) = calcTypeExpr tMap eD
calcTypeExpr tMap (AC.Rre _ _ _) = return AC.TUnit

calcTypeExpr tMap (AC.Rre _ _ _) = return AC.TUnit

-- direct casts
calcTypeExpr tMap (AC.TypeCast e (AC.UnitCast u)) = return $ AC.TFloat u
calcTypeExpr tMap (AC.TypeCast e (AC.WrapType (AC.LocalVar tName))) = return $ tMap Map.! tName
calcTypeExpr tMap (AC.TypeCast e (AC.UnwrapType (AC.LocalVar tName))) = return $ tMap Map.! tName

calcTypeExpr tMap (AC.Op op e) = do
    eT <- calcTypeExpr tMap e
    case op of
       -- Basic Ops
        BasicOp x | x `elem` [Add, Sub]                 -> typeFFtoF_USame eT   -- (f u1, f u1) -> f u1
        BasicOp x | x `elem` [Mul, Div]                 -> typeFFtoF_UAdd eT         -- (f u1, f u2) -> f u3
        BasicOp x | x `elem` [ACO.LT, LE, ACO.GT, GE, ACO.EQ, NEQ]  -> typeFFtoB_USame eT     -- (f u1, f u1) -> b
        BasicOp x | x `elem` [And, Or]                  -> typeBBtoB         -- (b, b) -> b
        BasicOp Not                                     -> typeBtoB          -- b -> b
        -- Math Ops
        MathOp x | x `elem` [ Sin, Cos, Tan, ASin, ACos, ATan, Exp, Exp2, Exp10, Pow10
                            , Log, Log2, Log10, LogB, Sqrt, Cbrt, ExpM1, Log1P
                            , SinH, CosH, TanH, ASinH, ACosH, ATanH
                            , Erf, ErfC, LGamma, TGamma] -> typeFtoF         -- f -> f
        -- MathOp SinCos                                   -> typeFtoFF         -- f -> (f,f)
        MathOp x | x `elem` [ATan2, Pow]                -> typeFFtoF         -- (f,f) -> f
        MathOp Hypot                                    -> typeFFtoF_USame eT   -- (f u1, f u1) -> f u1
        -- Other Ops
        OtherOp (UPow _)                                -> typeFtoF_UMul eT     -- f u1 -> f u2
        OtherOp (URoot _)                               -> typeFtoF_UMul eT     -- f u1 -> f u2
  where

    typeFFtoF   = return $ AC.TFloat U.NoUnit
    -- typeFFtoB   = return $ AC.TArr (AC.TTuple [AC.TFloat U.NoUnit, AC.TFloat U.NoUnit]) AC.TBool
    typeBBtoB   = return $ AC.TBool
    typeBtoB    = return $ AC.TBool
    typeFtoF    = return $ AC.TFloat U.NoUnit
    -- typeFtoFF   = return $ (AC.TTuple [AC.TFloat U.NoUnit, AC.TFloat U.NoUnit])

    -- dynamially determine the ret type given the input types and op semantics
    typeFtoF_USame eT = return eT
    typeFFtoF_USame (AC.TTuple [e1, e2]) = return e1

    typeFFtoF_UAdd (AC.TTuple [AC.TFloat u1, AC.TFloat u2]) = case op of
        (BasicOp Mul) -> return $ AC.TFloat $ U.addUnit u1 u2
        (BasicOp Div) -> return $ AC.TFloat $ U.subUnit u1 u2

    typeFFtoB_USame _ = return AC.TBool

    typeFtoF_UMul (AC.TFloat u1) = do
        case op of
            OtherOp (UPow exp)  -> return $ AC.TFloat $ U.mulUnit u1 exp
            OtherOp (URoot exp) -> AC.TFloat <$> U.divUnit u1 exp

-- should never occur
calcTypeExpr _ e = errorDump [MkSB e] "Cannot calculate type for expr" assert





