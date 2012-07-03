-----------------------------------------------------------------------------
--
-- Module      :  Lang.Core.UnitChecker
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | main algorithms and functionaliy for checking the units of a module
--
-----------------------------------------------------------------------------

module Lang.Core.UnitChecker (
unitCheck

) where

-- higher-level control
import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Error
import qualified Control.Conditional as Cond

-- fclabels stuff
import Control.Category
import Data.Label
import Prelude hiding ((.), id)

-- containers
import qualified Data.Foldable as DF
import qualified Data.Map as Map

-- other
import Text.Printf (printf)
import System.Log.Logger
import qualified System.Directory as Dir
import qualified System.FilePath as FP

-- Ode
import Utils.Utils
import Lang.Core.Units
import qualified Utils.OrdMap as OrdMap
import qualified UI.SysState as St

import qualified Lang.Core.AST as E
import qualified Lang.Module.AST as M
import qualified UI.SysState as St


-- functions
-- * unit checking
-- * dimenstion changing
-- * unit conversion
-- * module-level auto-conversion

-- TODO - need global modenv?, filedata?
unitCheck :: St.UnitsState -> M.Module E.Id -> MExcept (M.Module E.Id)

unitCheck unitsState mod@(M.LitMod exprMap modData) = do
    -- iterate over the expressions, and checking units along the way
    return mod

unitCheck unitsState mod@(M.FunctorMod args exprMap modData) = do
    return mod


-- atm, 3 types of units
-- Nothing - a value that does not have units, i.e. Bool
-- Just (UnitC []) - a value that may have units but is presently unoccupied, i.e. 3, dimless
-- Just (UnitC U) - a value with an actual unit, i.e. 3m

getUnitfromTMap :: M.TypeMap -> E.Id -> Maybe Unit
getUnitfromTMap tMap v = Map.lookup v tMap >>= getUnitForType

-- simple wrapper to extract the unit from a type, if possible
getUnitForType :: E.Type -> Maybe Unit
getUnitForType (E.TFloat mU) = mU
getUnitForType _ = Nothing


check :: St.UnitsState -> M.ModData -> (M.ExprMap E.Id) -> MExcept (M.TypeMap)
check uState modData exprMap = DF.foldlM checkTop (M.modTMap modData) (OrdMap.elems exprMap)

  where

    checkTop tMap (E.TopLet s (E.Bind bs) e) = do
        (tMap', t) <- checkExpr tMap e
        -- what to do with t
        return tMap'

    checkExpr :: M.TypeMap -> (E.Expr E.Id) -> MExcept (M.TypeMap, E.Type)
    checkExpr tMap (E.Var (E.LocalVar v)) = do
        -- lookup within tmap
        u <- case Map.lookup v tMap of
            Just u -> return u
            Nothing -> errorDump [MkSB v] $ printf "(UC01) Unknown var reference %s at UnitChecking stage" $ show v
        return (tMap, u)

    checkExpr tMap (E.Var (E.ModVar m v)) = return undefined

    checkExpr tMap (E.Lit l) = return $ (tMap, E.getLitType l)

    checkExpr tMap (E.Op op e) = do
        -- use the op to determien the built-in unit rules
        case op of
            E.Add   -> binEquals
            E.Sub   -> binEquals
            E.Mul   -> binMul
            E.Div   -> binDiv
            E.Mod   -> binEquals -- is this right?
            E.LT    -> binEquals
            E.LE    -> binEquals
            E.GT    -> binEquals
            E.GE    -> binEquals
            E.EQ    -> binEquals
            E.NEQ   -> binEquals
            _       -> return (tMap, E.TBool) -- how do we handle non-float types here?
      where
        -- encode the unit rules for arithmetic operations here
        binEquals = do
            (tMap', u1, u2) <- getExprUnits
            -- does this handle non-unit floats ??
            if u1 == u2 then return (tMap', E.TFloat (Just u1)) else throwError $ printf "Units error, units %s and %s are not of the same type" (show u1) (show u2)

        -- we allow any units to be mul/div
        binMul = do
            (tMap', u1, u2) <- getExprUnits
            -- need to add the units
            let u' = addUnit u1 u2
            return (tMap', E.TFloat (Just u'))

        binDiv = do
            (tMap', u1, u2) <- getExprUnits
            -- need to add the units
            let u' = subUnit u1 u2
            return (tMap', E.TFloat (Just u'))
        -- get the expr units
        getExprUnits = do
            (tMap', (E.TTuple [E.TFloat (Just u1), E.TFloat (Just u2)])) <- checkExpr tMap e
            return (tMap', u1, u2)


    checkExpr tMap e = errorDump [MkSB e] "(UC02) Unknown expr"





















