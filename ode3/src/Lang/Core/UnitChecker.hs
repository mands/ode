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
import Data.Maybe (fromJust)

-- Ode
import Utils.Utils
import Lang.Core.Units
import qualified Utils.OrdMap as OrdMap
import qualified UI.SysState as St

import qualified Lang.Core.AST as E
import qualified Lang.Module.AST as M
import qualified UI.SysState as St

-- Types ---------------------------------------------------------------------------------------------------------------

type TypeEnv = M.TypeMap
type ModTypeEnv = Map.Map (E.VarId E.Id) E.Type

-- Main Interface ------------------------------------------------------------------------------------------------------

-- functions
-- * unit constraint/checking & unification
-- * dimenstion changing / unit-polymorphism
-- * explicit and implicit unit conversion
-- needed data structures
-- * global modenv?, filedata?

unitCheck :: St.UnitsState -> M.Module E.Id -> MExcept (M.Module E.Id)

unitCheck unitsState mod@(M.LitMod exprMap modData) = do
    -- iterate over the expressions, and checking units along the way
    return mod

unitCheck unitsState mod@(M.FunctorMod args exprMap modData) = do
    return mod


-- Helper Funcs --------------------------------------------------------------------------------------------------------

-- atm, 2 types of units
-- Just (UnitC []) - a value that may have units but is presently unoccupied, i.e. 3, dimless
-- Just (UnitC U) - a value with an actual unit, i.e. 3m

getUnitfromTMap :: TypeEnv -> E.Id -> Maybe Unit
getUnitfromTMap tEnv v = Map.lookup v tEnv >>= getUnitForType

-- simple wrapper to extract the unit from a type, if possible
getUnitForType :: E.Type -> Maybe Unit
getUnitForType (E.TFloat u) = Just u
getUnitForType _ = Nothing

-- we explicitly consider a failed lookup here to be a compiler error, they should have all been found during renaming/type-checking
getType :: TypeEnv -> E.Id -> MExcept E.Type
getType tEnv v = case Map.lookup v tEnv of
                    Just t -> return t
                    Nothing -> errorDump [MkSB v] $ printf "(UC01) Unknown var reference %s at UnitChecking stage" $ show v


throwUnitsError :: (Show a) => String -> a -> a -> MExcept b
throwUnitsError loc u1 u2 =
    throwError $ printf "Units mismatch in %s, expected unit %s, actual unit %s" (show loc) (show u1) (show u2)


-- Unit Constraint Generation -------------------------------------------------------------------------------------------

constrain :: St.UnitsState -> M.ModData -> (M.ExprMap E.Id) -> MExcept (TypeEnv)
constrain uState modData exprMap =
    -- fold over exprs, using the existing type-map as a our reference
    DF.foldlM consTop (M.modTMap modData) (OrdMap.elems exprMap)

  where

    -- | Main entry point over checking a top-level
    consTop tEnv (E.TopLet s (E.Bind bs) e) = do
        (tEnv', eT) <- consExpr tEnv e

        -- update and return the modified tEnv
        case eT of
            -- true if tuple on both side of same size, if so unplack and treat as indivudual elems
            (E.TTuple ts) | (length bs == length ts) -> return $ foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv' (zip bs ts)
            -- true for individual elems, handle same as tuple above
            t | length bs == 1 -> return $ Map.insert (head bs) eT tEnv'
            _ -> errorDump [MkSB bs, MkSB eT, MkSB tEnv'] "(UC) - toplet shit\n"

    -- | A recusive pass over the expression AST - simply no fancy inference required, simply use the existing and
    -- explicit unit information against the set unit rules
    consExpr :: TypeEnv -> (E.Expr E.Id) -> MExcept (TypeEnv, E.Type)
    consExpr tEnv (E.Var (E.LocalVar v)) = do
        -- lookup within tmap
        t <- getType tEnv v
        return (tEnv, t)

    consExpr tEnv (E.Var (E.ModVar m v)) = return undefined
        -- we may need up update the mTMap here??

    -- simple static lookup for literals
    -- consExpr tEnv (E.Lit l) = return $ (tEnv, E.getLitType l)

    consExpr tEnv (E.App (E.LocalVar f) e) = do
        -- need to lookup fT, get unit for e, make sure units match up
        -- we could actually convert here if we wanted to
        (E.TArr fromT toT) <- getType tEnv f
        (tEnv', eT) <- consExpr tEnv e
        if fromT == eT
            then return (tEnv', toT)
            else throwUnitsError "Calling component" fromT eT


    consExpr tEnv (E.App mv@(E.ModVar m v) e) = undefined
        -- we may need up update the mTMap here??

    -- simply check the e
    consExpr tEnv (E.Abs arg e) = consExpr tEnv e


    consExpr tEnv (E.Let s (E.Bind bs) e1 e2) = do
        -- check the e1 and e2, update the tEnv, return e2,
        (tEnv', e1T) <- consExpr tEnv e1
        tEnv'' <- case e1T of
            -- true if tuple on both side of same size, if so unplack and treat as indivudual elems
            (E.TTuple ts) | (length bs == length ts) -> return $ foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv' (zip bs ts)
            -- true for individual elems, handle same as tuple above
            t | (length bs == 1) -> return $ Map.insert (head bs) e1T tEnv'
            _ -> errorDump [MkSB bs, MkSB e1T, MkSB tEnv'] "(UC) - let shit\n"
        consExpr tEnv'' e2


    -- most unit updating logic is in here, using the built-in rules
    consExpr tEnv (E.Op op e) = do
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
            _       -> return (tEnv, E.TBool) -- how do we handle non-float types here?
      where
        -- encode the unit rules for arithmetic operations here
        binEquals = do
            (tEnv', u1, u2) <- getExprUnits
            -- does this handle non-unit floats ??
            if u1 == u2
                then return (tEnv', E.TFloat u1)
                else throwUnitsError "arithmetic statement" u1 u2

        -- we allow any units to be mul/div
        binMul = do
            (tEnv', u1, u2) <- getExprUnits
            -- need to add the units
            let u' = addUnit u1 u2
            return (tEnv', E.TFloat u')

        binDiv = do
            (tEnv', u1, u2) <- getExprUnits
            -- need to add the units
            let u' = subUnit u1 u2
            return (tEnv', E.TFloat u')

        -- get the expr units
        getExprUnits = do
            (tEnv', (E.TTuple [E.TFloat u1, E.TFloat u2])) <- consExpr tEnv e
            return (tEnv', u1, u2)

    consExpr tEnv (E.If eB eT eF) = do
        -- check units the same for T and F
        (tEnv', eTT) <- consExpr tEnv eT
        (tEnv'', eFT) <- consExpr tEnv' eF

        if eTT == eFT
            then return (tEnv', eTT)
            else throwUnitsError "If statement" eTT eFT

    -- fold over the expressions, collect the units and gerneate a new tuple type at the end
    consExpr tEnv (E.Tuple es) = liftM checkTuple (DF.foldlM checkElem (tEnv, []) es)
      where
        checkElem (tEnv, eTs) e = consExpr tEnv e >>= (\(tEnv', eT) -> return (tEnv', eT:eTs))
        checkTuple (tEnv, eTs) = (tEnv, E.TTuple (reverse eTs))

    consExpr tEnv (E.Ode (E.LocalVar v) eD) = undefined
        -- not sure, think eD needs to be /s, and actual integrated val removes that

    consExpr tEnv (E.Rre (E.LocalVar src) (E.LocalVar dest) _) = undefined
        -- think can't have time involved, both have to be same unit??

    consExpr tEnv (E.ConvCast e toU) = do
        -- need to get unit of e
        (tEnv', eT) <- consExpr tEnv e
        -- type-checker ensures this will be a float, won't fail
        let eU = fromJust $ getUnitForType eT
        -- make sure is within same dim as u, and a convert exists
        _ <- calcConvExpr eU toU (get St.lUnitDimEnv uState) (get St.lConvEnv uState)
        -- return the new casted unit
        return (tEnv', E.TFloat toU)

    consExpr tEnv e = errorDump [MkSB e] "(UC02) Unknown expr"


-- Unit Conversion -----------------------------------------------------------------------------------------------------
-- we can do this at component and module level boundaries - basically anywhere where we have a pre-defined input/ouput unit

-- TODO














