-----------------------------------------------------------------------------
--
-- Module      :  Lang.Core.TypeChecker.Unification
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

module Lang.Core.TypeChecker.Unification (
unify
) where

-- higher-level control, Monads, etc.
import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import qualified Control.Monad.State as S
import Control.Monad.Error
import qualified Control.Conditional as Cond

-- fclabels stuff
import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)

-- containers
import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Data.Set as Set

-- other
import Data.Maybe (fromJust)
import Text.Printf (printf)


-- Ode
import Utils.Utils
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap

import qualified UI.SysState as SysS
import Lang.Common.AST
import qualified Lang.Core.AST as E
import qualified Lang.Module.AST as M
import qualified Lang.Core.Units as U

import Lang.Core.TypeChecker.Common

-- TODO
-- * check substiution and occursCheck rules for units
-- * simplify equality checks

-- Types ---------------------------------------------------------------------------------------------------------------

type UnifyM = S.StateT (TypeVarEnv, UnitVarEnv) MExcept

-- Helper Functions ----------------------------------------------------------------------------------------------------

-- | replaces all occurances of tVar x with y in tCons where possible using the current tVMap and uVMap
subStack :: TypeCons -> UnifyM TypeCons
subStack tCons = do
    (tVEnv, uVEnv) <- S.get
    subStack' tVEnv uVEnv
  where
    subStack' tVEnv uVEnv = return $ tCons { conEqualS = conEqualS', conSumS = conSumS', conSameDimS = conSameDimS' }
      where
        conEqualS' = Set.map (\(ConEqual a b) -> (ConEqual (updateTypes a) (updateTypes b))) $ conEqualS tCons
        conSumS' = Set.map (\(ConSum a b c) -> (ConSum (updateUnits a) (updateUnits b) (updateUnits c))) $ conSumS tCons
        conSameDimS' = Set.map (\(ConSameDim a b) -> (ConSameDim (updateUnits  a) (updateUnits b))) $ conSameDimS tCons

        updateTypes = E.mapType (\t -> case t of
            t1@(E.TVar tV) -> Map.findWithDefault t1 tV tVEnv
            t1@(E.TFloat (U.UnitVar uV)) -> maybe t1 (E.TFloat) $ Map.lookup uV uVEnv
            t1 -> t1)

        updateUnits u1@(U.UnitVar uV) = Map.findWithDefault u1 uV uVEnv
        updateUnits u1 = u1

-- | replace all occurances of t1->t2 in the set of constraints
subEqualS :: E.Type -> E.Type -> ConEqualS -> ConEqualS
subEqualS t1 t2 = Set.map (\(ConEqual a b) -> (ConEqual (subTTerm t1 t2 a) (subTTerm t1 t2 b)))

-- | replace all occurances of u1->u2 in the set of constraints
subSumS :: U.Unit -> U.Unit -> ConSumS -> ConSumS
subSumS u1 u2 = Set.map (\(ConSum a b c) -> (ConSum (subUTerm u1 u2 a) (subUTerm u1 u2 b) (subUTerm u1 u2 c)))

-- | replace all occurances of u1->u2 in the set of constraints
subSameDimS :: U.Unit -> U.Unit -> ConSameDimS -> ConSameDimS
subSameDimS u1 u2 = Set.map (\(ConSameDim a b) -> (ConSameDim (subUTerm u1 u2 a) (subUTerm u1 u2 b)))


-- | replaces all occurances of (tVar x) with y in tEnv, then add [x->y] to the tEnv
-- TODO - prob should have a separate env for UnitVar Ids -> Units/Types
subAddType t1 t2 tEnv = case t1 of
    E.TVar tV1 -> Map.insert tV1 t2 tEnv'
    _ -> tEnv'
  where
    tEnv' = Map.map (subTTerm t1 t2) tEnv

-- | replaces all occurances of x with y in the tTerm t
subTTerm x y t@(E.TTuple ts)
    | t == x = y
    | otherwise = E.TTuple $ map (subTTerm x y) ts
subTTerm x y t@(E.TArr fromT toT)
    | t == x = y
    | otherwise = E.TArr (subTTerm x y fromT) (subTTerm x y toT)
subTTerm x y t = if t == x then y else t

-- | checks that tVar x does not exist in tTerm t, stop recursive substitions
occursCheck :: E.Type -> E.Type -> Bool
occursCheck x t@(E.TTuple ts)
    | t == x = True
    | otherwise = any (occursCheck x) ts
occursCheck x t@(E.TArr fromT toT)
    | t == x = True
    | otherwise = (occursCheck x fromT) || (occursCheck x toT)
occursCheck x t = if t == x then True else False

-- TODO - inline these trivial functions ?
-- | only need to test equality on the top-level of the unit, as composite units are not allowed
subAddUnit u1 u2 uEnv =
    case u1 of
        (U.UnitVar uV1) -> Map.insert uV1 u2 uEnv'
        _ -> uEnv'
  where
    uEnv' = Map.map (subUTerm u1 u2) uEnv

-- | replaces all occurances of u1 with u2 in the unit u3
subUTerm :: U.Unit -> U.Unit -> U.Unit -> U.Unit
subUTerm u1 u2 u3 = if u1 == u3 then u2 else u3

unitOccursCheck :: U.Unit -> U.Unit -> Bool
unitOccursCheck = (==)


-- Main functions ------------------------------------------------------------------------------------------------------

-- | unify takes a set of type contraints and attempts to unify all types, inc TVars
-- based on HM - standard constraint unification algorithm
-- Do we want to return the set of remaining constraints as well??
unify :: SysS.UnitsState -> TypeCons -> MExcept (TypeVarEnv, UnitVarEnv)
unify uState tCons = snd <$> S.runStateT unifyM (Map.empty, Map.empty)
  where
    --trace' [MkSB tCons] "Initial Unify tCons" $
    unifyM = do
        -- return set should be empty
        conEqualS' <- unifyEquals (conEqualS tCons)
        tCons' <- subStack (tCons { conEqualS = conEqualS' })
        unitLoop tCons'

    -- need to repeat unifyUnits until we can infer no more from tCons
    unitLoop tCons = do
        tCons' <- unifyUnits tCons
        if (tCons /= tCons') then unitLoop tCons' else return tCons'

    unifyUnits tCons = do
        conSumS' <- unifySum (conSumS tCons)
        tCons' <- subStack (tCons { conSumS = conSumS' })

        conSameDimS' <- unifySameDim uState (conSameDimS tCons')
        tCons'' <- subStack (tCons' { conSameDimS = conSameDimS' })
        return tCons''

-- | Unification (Full) and Checking (Some) for Equal rule
unifyEquals :: ConEqualS ->  UnifyM ConEqualS
unifyEquals conEqualS = --trace' [MkSB tCons, MkSB tEnv] "Unify iteration" $ case (Set.minView tCons) of
                case Set.minView conEqualS of -- get a constraint from the set
                    Just (cons, conEqualS') -> processEqual cons conEqualS' >>= unifyEquals
                    -- we're done, no constraints left in the set
                    Nothing -> return conEqualS
  where

    processEqual :: ConEqual -> ConEqualS -> UnifyM ConEqualS
    -- two equal types - can by typevars, base units, unitVars, composites, anything that is fully equal
    -- unifyEquals' (ConEqual x y) st | (x == y) = return st

    -- two equal ids - remove from set and ignore
    processEqual (ConEqual (E.TVar xId) (E.TVar yId)) st
       | (xId == yId) = return st

    -- replace all x with y
    processEqual (ConEqual x@(E.TVar xId) y) st
        | not (occursCheck x y) = do
            (tEnv, _) <- S.get
            -- We have to check the TypeEnv here to see if the TypeVar has already been subsituted, this is because,
            -- even though we run the subAddMap/subStack functions that will update the map and set, we may still have
            -- multiple substitutions for a tVar due to case-analysis on the current rule that may be a composite case
            -- i.e. TTArr or TTuple, hence the Equality rules within them would not have been updated
            case Map.lookup xId tEnv of
                 -- the tvar has already been updated at some point within this
                 -- ConEqual rule, use this and recheck, reinsert a new equality constraint
                Just xT -> return $ Set.insert (ConEqual xT y) st
                --Just xT -> uCon (xT, y) (tCons, tEnv) -- can also, but neater to reinsert into the tCons
                -- tvar doesn't exist, add and sub
                Nothing -> do
                    _ <- S.modify (\(tEnv, uEnv) -> (subAddType x y tEnv, uEnv))
                    return $ subEqualS x y st

    -- replace all y with x
    processEqual (ConEqual x y@(E.TVar _)) st = processEqual (ConEqual y x) st
    --        | not (occursCheck y x) = return (subStack y x tCons, subMap y x tEnv)

    processEqual (ConEqual (E.TArr x1 x2) (E.TArr y1 y2)) st = do
        st' <- processEqual (ConEqual x1 y1) st
        processEqual (ConEqual x2 y2) st'

    processEqual (ConEqual (E.TTuple xs) (E.TTuple ys)) st | (length xs == length ys) =
        DF.foldlM (\st (x, y) -> processEqual (ConEqual x y) st) st (zip xs ys)


    -- Explicit UnitVar equality handling
    -- uV = uV
    processEqual (ConEqual (E.TFloat (U.UnitVar uV1)) (E.TFloat u2@(U.UnitVar uV2))) st | (uV1 == uV2) = return st
    -- uV = u
    processEqual (ConEqual t1@(E.TFloat (U.UnitVar uV1)) t2@(E.TFloat u2@(U.UnitC _))) st = do
        -- no occursCheck needed, no composite types
        -- simply add to tEnv and replace in tCons
        (tEnv, _) <- S.get
        case Map.lookup uV1 tEnv of
            -- the tvar has already been updated at some point within this
            -- ConEqual rule, use this and recheck, reinsert a new equality constraint
            Just t1' -> return $ Set.insert (ConEqual t1' t2) st
            --Just xT -> uCon (xT, y) (tCons, tEnv) -- can also, but neater to reinsert into the tCons
            -- tvar doesn't exist, add and sub
            Nothing -> S.modify (\(tEnv, uEnv) -> (subAddType t1 t2 tEnv, uEnv)) >> return (subEqualS t1 t2 st)
    -- u = uV
    processEqual (ConEqual t1@(E.TFloat u2@(U.UnitC _)) t2@(E.TFloat (U.UnitVar uV1))) st = processEqual (ConEqual t2 t1) st

    -- two equal types - can by typevars, base units, unitVars, composites, anything that is fully equal
    -- TODO - enable at top of cases
    processEqual (ConEqual x y) st | (x == y) = return st
    -- uCon (x, y, uRule) st = undefined -- are these needed?

    -- can't unify types
    processEqual (ConEqual x y) st = trace' [MkSB x, MkSB y, MkSB st] "Type Error" $ throwError (printf "(TC01) - cannot unify %s and %s" (show x) (show y))

-- | Unification (Some) and Checking (Full) for Sum rule
unifySum :: ConSumS ->  UnifyM ConSumS
unifySum conSumS = unifySumLoop conSumS
  where

    unifySumLoop :: ConSumS -> UnifyM ConSumS
    unifySumLoop startConSumS = unifySumLoop' startConSumS Set.empty
      where
        -- need to keep looping until no-more changes occur
        unifySumLoop' :: ConSumS -> ConSumS -> UnifyM ConSumS
        unifySumLoop' curSumS newSumS = case Set.minView curSumS of
            Just (con, curSumS') -> processSum con curSumS' newSumS >>= uncurry unifySumLoop' -- continue loop
            -- set empty, check new set if something more to iterate on, else quit
            -- TODO - check we are comparing the right sets here
            Nothing | (newSumS /= startConSumS) -> unifySumLoop newSumS -- somthing changed, restart loop again from top
            Nothing | otherwise -> return newSumS -- no change, have infered all we could, return the remaining

    -- actually process the sum rule
    -- will always be a FLoat val
    processSum :: ConSum -> ConSumS -> ConSumS -> UnifyM (ConSumS, ConSumS)

    -- 3 uVars - can do fuck all, copy to newSums
    processSum con@(ConSum (U.UnitVar _) (U.UnitVar _) (U.UnitVar _)) curS newS = return (curS, Set.insert con newS)

    -- 2 unit vars - can only do something if have a NoUnit, otherwise fuck all
    -- uV uV NU
    processSum (ConSum u1@(U.UnitVar _) u2@(U.UnitVar _) U.NoUnit) curS newS = replaceUnit u1 u2 curS newS
    -- uV NU uV
    processSum (ConSum u1@(U.UnitVar _) U.NoUnit u2@(U.UnitVar _)) curS newS = replaceUnit u1 u2 curS newS
    -- NU uV uV
    processSum (ConSum U.NoUnit u1@(U.UnitVar _) u2@(U.UnitVar _)) curS newS = replaceUnit u1 u2 curS newS

    -- 1 unit var -- can infer and replace the correct unit
    -- u u uV
    processSum (ConSum u1@(U.UnitC _) u2@(U.UnitC _) uV3@(U.UnitVar _)) curS newS = replaceUnit uV3 (U.addUnit u1 u2) curS newS
    -- u uV u
    processSum (ConSum u1@(U.UnitC _) uV2@(U.UnitVar _) u3@(U.UnitC _)) curS newS = replaceUnit uV2 (U.subUnit u3 u1) curS newS
    -- uV u u
    processSum (ConSum uV1@(U.UnitVar _) u2@(U.UnitC _) u3@(U.UnitC _)) curS newS = replaceUnit uV1 (U.subUnit u3 u2) curS newS

    processSum con curS newS = errorDump [MkSB con, MkSB curS, MkSB newS] "Found an invalid ConSum"

    -- update all units x->y
    -- where x = Float (UnitVar uID), y = Float Unit
    replaceUnit :: U.Unit -> U.Unit -> ConSumS -> ConSumS -> UnifyM (ConSumS, ConSumS)
    replaceUnit u1 u2 curS newS
        | not (unitOccursCheck u1 u2) = do
                -- unlike the unifyEqual rule, we do not need to check the TypeMap to see if uVar already exists, as we
                -- already substitute the map/tCons and there are not compositite cases where this subsitituion would not be sufficcent
                -- just add and sub
                S.modify (\(tEnv, uEnv) -> (tEnv, subAddUnit u1 u2 uEnv))
                let curS' = subSumS u1 u2 curS
                let newS' = subSumS u1 u2 newS
                return (curS', newS')
        | otherwise = return (curS, newS)



-- | Unification (Minimal) and Checking (Full) for SameDim rule
unifySameDim :: SysS.UnitsState -> ConSameDimS ->  UnifyM ConSameDimS
unifySameDim uState conSameDimS = unifySameDimLoop conSameDimS
  where

    unifySameDimLoop :: ConSameDimS -> UnifyM ConSameDimS
    unifySameDimLoop startS = unifySameDimLoop' startS Set.empty
      where
        -- need to keep looping until no-more changes occur
        unifySameDimLoop' :: ConSameDimS -> ConSameDimS -> UnifyM ConSameDimS
        unifySameDimLoop' curS newS = case Set.minView curS of
            Just (con, curS') -> processSameDim con curS' newS >>= uncurry unifySameDimLoop' -- continue loop
            -- set empty, check new set if something more to iterate on, else quit
            -- TODO - check we are comparing the right sets here
            Nothing | (newS /= startS) -> unifySameDimLoop newS -- somthing changed, restart loop again from top
            Nothing | otherwise -> return newS -- no change, have infered all we could, return the remaining

    processSameDim :: ConSameDim -> ConSameDimS -> ConSameDimS -> UnifyM (ConSameDimS, ConSameDimS)

    -- both same uVar - ignore
    processSameDim (ConSameDim u1@(U.UnitVar uV1) u2@(U.UnitVar uV2)) curS newS | uV1 == uV2 = return (curS, newS)

    -- 2 uVars - do fuck all
    processSameDim (ConSameDim u1@(U.UnitVar _) u2@(U.UnitVar _)) curS newS = return (curS, newS)

    -- 1 uVar - can only do something if have a NoUnit, have to sub in
    -- TODO - check, does this ever occur?
    processSameDim (ConSameDim u1@(U.UnitVar _) U.NoUnit) curS newS = replaceUnit u1 U.NoUnit curS newS
    processSameDim (ConSameDim U.NoUnit u2@(U.UnitVar _)) curS newS = replaceUnit u2 U.NoUnit curS newS

    -- 1 uVar - other is a unit, fuck all
    processSameDim (ConSameDim u1@(U.UnitVar _) u2@(U.UnitC _)) curS newS = return (curS, newS)
    processSameDim (ConSameDim u1@(U.UnitC _) u2@(U.UnitVar _)) curS newS = return (curS, newS)

    -- both units or nounits - check they are equal dimension
    processSameDim (ConSameDim U.NoUnit U.NoUnit) curS newS = return (curS, newS)

    processSameDim (ConSameDim u1@(U.UnitC _) u2@(U.UnitC _)) curS newS = do
        -- make sure is within same dim as u, and a conversion path exists
        lift $ U.calcConvExpr u1 u2 (L.get SysS.lUnitDimEnv uState) (L.get SysS.lConvEnv uState)
        return (curS, newS)

    -- update all units x->y
    -- where x = Float (UnitVar uID), y = Float Unit
    replaceUnit u1 u2 curS newS
        | not (unitOccursCheck u1 u2) = do
                -- unlike the unifyEqual rule, we do not need to check the TypeMap to see if uVar already exists, as we
                -- already substitute the map/tCons and there are not compositite cases where this subsitituion would not be sufficcent
                -- just add and sub
                S.modify (\(tEnv, uEnv) -> (tEnv, subAddUnit u1 u2 uEnv))
                let curS' = subSameDimS u1 u2 curS
                let newS' = subSameDimS u1 u2 newS
                return (curS', newS')
        | otherwise = return (curS, newS)
