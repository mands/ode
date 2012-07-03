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

-- we explicitly consider a failed lookup here to be a compiler error, they should have all been found during renaming/type-checking
getType :: M.TypeMap -> E.Id -> MExcept E.Type
getType tMap v = case Map.lookup v tMap of
                    Just t -> return t
                    Nothing -> errorDump [MkSB v] $ printf "(UC01) Unknown var reference %s at UnitChecking stage" $ show v


throwUnitsError :: (Show a) => String -> a -> a -> MExcept b
throwUnitsError loc u1 u2 =
    throwError $ printf "Units mismatch in %s, expected unit %s, actual unit %s" (show loc) (show u1) (show u2)


check :: St.UnitsState -> M.ModData -> (M.ExprMap E.Id) -> MExcept (M.TypeMap)
check uState modData exprMap = DF.foldlM checkTop (M.modTMap modData) (OrdMap.elems exprMap)

  where

    -- | Main entry point over checking a top-level
    checkTop tMap (E.TopLet s (E.Bind bs) e) = do
        (tMap', t) <- checkExpr tMap e
        -- what to do with t
        return tMap'


    -- | A recusive pass over the expression AST - simply no fancy inference required, simply use the existing and
    -- explicit unit information against the set unit rules
    checkExpr :: M.TypeMap -> (E.Expr E.Id) -> MExcept (M.TypeMap, E.Type)
    checkExpr tMap (E.Var (E.LocalVar v)) = do
        -- lookup within tmap
        t <- getType tMap v
        return (tMap, t)

    checkExpr tMap (E.Var (E.ModVar m v)) = return undefined
        -- we may need up update the mTMap here??

    -- simple static lookup for literals
    checkExpr tMap (E.Lit l) = return $ (tMap, E.getLitType l)

    checkExpr tMap (E.App (E.LocalVar f) e) = do
        -- need to lookup fT, get unit for e, make sure units match up
        -- we could actually convert here if we wanted to
        (E.TArr fromT toT) <- getType tMap f
        (tMap', eT) <- checkExpr tMap e
        if fromT == eT
            then return (tMap', toT)
            else throwUnitsError "Calling component" fromT eT


    checkExpr tMap (E.App mv@(E.ModVar m v) e) = undefined
        -- we may need up update the mTMap here??

    -- simply check the e
    checkExpr tMap (E.Abs arg e) = checkExpr tMap e


    checkExpr tMap (E.Let s (E.Bind bs) e1 e2) = do
        -- check the e1 and e2, update the tMap, return e2,
        (tMap', e1T) <- checkExpr tMap e1
        tMap'' <- case e1T of
            -- true if tuple on both side of same size, if so unplack and treat as indivudual elems
            (E.TTuple ts) | (length bs == length ts) -> return $ foldl (\tMap (b, t) -> Map.insert b t tMap) tMap' (zip bs ts)
            -- true for individual elems, handle same as tuple above
            t | (length bs == 1) -> return $ Map.insert (head bs) e1T tMap'
            _ -> errorDump [MkSB bs, MkSB e1T, MkSB tMap'] "(UC) - let shit\n"
        checkExpr tMap'' e2


    -- most unit updating logic is in here, using the built-in rules
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
            if u1 == u2
                then return (tMap', E.TFloat (Just u1))
                else throwUnitsError "arithmetic statement" u1 u2

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

    checkExpr tMap (E.If eB eT eF) = do
        -- check units the same for T and F
        (tMap', eTT) <- checkExpr tMap eT
        (tMap'', eFT) <- checkExpr tMap' eF

        if eTT == eFT
            then return (tMap', eTT)
            else throwUnitsError "If statement" eTT eFT

    -- fold over the expressions, collect the units and gerneate a new tuple type at the end
    checkExpr tMap (E.Tuple es) = liftM checkTuple (DF.foldlM checkElem (tMap, []) es)
      where
        checkElem (tMap, eTs) e = checkExpr tMap e >>= (\(tMap', eT) -> return (tMap', eT:eTs))
        checkTuple (tMap, eTs) = (tMap, E.TTuple (reverse eTs))

    checkExpr tMap (E.Ode (E.LocalVar v) eD) = undefined
        -- not sure, think eD needs to be /s, and actual integrated val removes that

    checkExpr tMap (E.Rre (E.LocalVar src) (E.LocalVar dest) _) = undefined
        -- think can't have time involved, both have to be same unit??

    checkExpr tMap (E.ConvCast e toU) = do
        -- need to get unit of e
        (tMap', eT) <- checkExpr tMap e
        -- type-checker ensures this will be a float, won't fail
        let eU = fromJust $ getUnitForType eT
        -- make sure is within same dim as u, and a convert exists
        _ <- calcConvExpr eU toU (get St.lUnitDimEnv uState) (get St.lConvEnv uState)
        -- return the new casted unit
        return (tMap', (E.TFloat $ Just toU))

    checkExpr tMap e = errorDump [MkSB e] "(UC02) Unknown expr"



















