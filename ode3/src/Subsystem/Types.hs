-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Types
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
{-# LANGUAGE PatternGuards #-}
module Subsystem.Types (
calcTypeExpr
) where


import qualified Data.Map as Map
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT

import Utils.CommonImports
import Utils.MonadSupply

import AST.Common as ACO
import qualified AST.Core as AC
import AST.Module
import qualified Subsystem.Units as U



-- Type Helper Funcs -----------------------------------------------------------------------------------------------------
-- TODO - should this be moved??

-- calculates the type of any abritary expression via recusrive descent and tMap lookup
-- we need this as we only store types on at (top-)let points
-- assumes both that type-checking hsa completed (hence non-poly typemap is present) and modules are inlined/not presesent
-- also assumes that types within let-bound expressions are newer/take prcedence over tMap

-- Takes as input the current type-map and the unit of the time keyword
calcTypeExpr :: (TypeMap, U.Unit) -> AC.Expr Id -> MExcept AC.Type
calcTypeExpr (tMap, _) (AC.Var (AC.LocalVar v) Nothing) = return $ tMap Map.! v
calcTypeExpr (tMap, _) (AC.Var (AC.LocalVar v) (Just recId)) | (AC.TRecord ts) <- tMap Map.! v = return $ ts Map.! recId

calcTypeExpr (tMap, _) (AC.App (AC.LocalVar f) _) | AC.TArr eT toT <- (tMap Map.! f) = return toT

-- what is the type of the arg? AC.TArr Unit (calcTypeExpr tMap e)
-- calcTypeExpr tMap (AC.Abs arg e) = undefined -- calcTypeExpr tMap e

-- the t here holds the type of e1, not e2, hence have to calc e2
calcTypeExpr (tMap, tUnit) (AC.Let s t (b:[]) e1 e2) = calcTypeExpr (Map.insert b t tMap, tUnit) e2
calcTypeExpr (tMap, tUnit) (AC.Let s (AC.TTuple ts) bs e1 e2) = calcTypeExpr (tMap', tUnit) e2
  where
    tMap' = foldl (\tMap (b,t) -> Map.insert b t tMap) tMap $ zip bs ts

calcTypeExpr (_, tUnit) (AC.Lit l) = case l of
    AC.Boolean _ -> return AC.TBool
    AC.Num _ u -> return $ AC.TFloat u
    AC.Time -> return $ AC.TFloat tUnit
    AC.Wiener -> return $ AC.TFloat U.NoUnit -- is the unit of wiener correct?
    AC.Unit -> return AC.TUnit

calcTypeExpr st (AC.If eB eT eF) = calcTypeExpr st eT

-- are these unpacked yet?
calcTypeExpr st (AC.Tuple es) = AC.TTuple <$> DT.mapM (calcTypeExpr st) es
-- is a literal record, record this within the type
calcTypeExpr st (AC.Record nEs) = AC.TRecord <$> DT.mapM (calcTypeExpr st) nEs

-- sim ops - types of Odes & Sdes = type of eD, type of RRE = Unit
calcTypeExpr st (AC.Ode lv@(AC.LocalVar _) eD) = calcTypeExpr st eD
calcTypeExpr st (AC.Sde lv@(AC.LocalVar _) eW eD) = calcTypeExpr st eD
calcTypeExpr _ (AC.Rre _ _ _) = return AC.TUnit

-- direct casts
calcTypeExpr _ (AC.TypeCast e (AC.UnitCast u)) = return $ AC.TFloat u
calcTypeExpr (tMap, tUnit) (AC.TypeCast e (AC.WrapType (AC.LocalVar tName))) = return $ tMap Map.! tName
calcTypeExpr (tMap, tUnit) (AC.TypeCast e (AC.UnwrapType (AC.LocalVar tName))) = return $ tMap Map.! tName

calcTypeExpr st (AC.Op op e) = do
    eT <- calcTypeExpr st e
    case op of
       -- Basic Ops
        BasicOp x | x `elem` [Add, Sub]                 -> typeFFtoF_USame eT    -- (f u1, f u1) -> f u1
        BasicOp x | x `elem` [Mul, Div]                 -> typeFFtoF_UAdd eT     -- (f u1, f u2) -> f u3
        BasicOp x | x `elem` [ACO.LT, LE, ACO.GT, GE, ACO.EQ, NEQ]  -> typeFFtoB_USame eT     -- (f u1, f u1) -> b
        BasicOp x | x `elem` [And, Or]                  -> typeBBtoB             -- (b, b) -> b
        BasicOp Not                                     -> typeBtoB              -- b -> b
        -- Math Ops
        MathOp x | x `elem` [ Sin, Cos, Tan, ASin, ACos, ATan, Exp, Exp2, Exp10, Pow10
                            , Log, Log2, Log10, LogB, Sqrt, Cbrt, ExpM1, Log1P
                            , SinH, CosH, TanH, ASinH, ACosH, ATanH
                            , Erf, ErfC, LGamma, TGamma] -> typeFtoF             -- f -> f
        -- MathOp SinCos                                   -> typeFtoFF         -- f -> (f,f)
        MathOp x | x `elem` [ATan2, Pow]                -> typeFFtoF             -- (f,f) -> f
        MathOp Hypot                                    -> typeFFtoF_USame eT    -- (f u1, f u1) -> f u1
        MathOp x | x `elem` [FAbs, Floor, Ceil, Round]  -> typeFtoF_USame eT     -- f u1 -> f u1
        -- Other Ops
        OtherOp (UPow _)                                -> typeFtoF_UMul eT      -- f u1 -> f u2
        OtherOp (URoot _)                               -> typeFtoF_UMul eT      -- f u1 -> f u2
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





