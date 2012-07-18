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
-- * spearate unitEnv
-- * Update rules to use correct types - TType or Unit
-- * check substiution and occursCheck rules for units
-- * simplify equality checks

-- Types ---------------------------------------------------------------------------------------------------------------

type UnifyM = S.StateT TypeVarEnv MExcept

-- Helper Functions ----------------------------------------------------------------------------------------------------


-- | replaces all occurances of tVar x with y in tCons
subStack :: E.Type -> E.Type -> TypeCons -> TypeCons
subStack x y tCons = tCons { consEquals = consEquals', consSums = consSums', consSameDims = consSameDims' }
  where
    consEquals' = Set.map (\(ConsEqual a b) -> (ConsEqual (subTTerm x y a) (subTTerm x y b))) $ consEquals tCons
    consSums' = Set.map (\(ConsSum a b c) -> (ConsSum (subTTerm x y a) (subTTerm x y b) (subTTerm x y c))) $ consSums tCons
    consSameDims' = Set.map (\(ConsSameDim a b) -> (ConsSameDim (subTTerm x y a) (subTTerm x y b))) $ consSameDims tCons

-- | replaces all occurances of (tVar x) with y in tEnv, then add [x->y] to the tEnv
-- TODO - prob should have a separate env for UnitVar Ids -> Units/Types
subAddMap x@(E.TVar xId) y tEnv =
    case x of
        E.TVar xId -> Map.insert xId y tEnv'
        E.TFloat (U.UnitVar uId) -> Map.insert uId y tEnv'
        _ -> tEnv'
  where
    tEnv' = Map.map (subTTerm x y) tEnv

-- | checks that tVar x does not exist in tTerm t, stop recursive substitions
occursCheck :: E.Type -> E.Type -> Bool
occursCheck x t@(E.TTuple ts)
    | t == x = True
    | otherwise = any (occursCheck x) ts
occursCheck x t@(E.TArr fromT toT)
    | t == x = True
    | otherwise = (occursCheck x fromT) || (occursCheck x toT)
occursCheck x t = if t == x then True else False

-- | replaces all occurances of x with y in the tTerm t
subTTerm x y t@(E.TTuple ts)
    | t == x = y
    | otherwise = E.TTuple $ map (subTTerm x y) ts
subTTerm x y t@(E.TArr fromT toT)
    | t == x = y
    | otherwise = E.TArr (subTTerm x y fromT) (subTTerm x y toT)
subTTerm x y t = if t == x then y else t

-- Main functions ------------------------------------------------------------------------------------------------------

-- | unify takes a set of type contraints and attempts to unify all types, inc TVars
-- based on HM - standard constraint unification algorithm
unify :: SysS.UnitsState -> TypeCons -> MExcept TypeVarEnv
unify uState tCons = snd <$> S.runStateT unifyM Map.empty
  where
    --trace' [MkSB tCons] "Initial Unify tCons" $
    unifyM = unifyEquals tCons >>= unifySum >>= unifySameDim uState


    -- loop to unify equality
unifyEquals :: TypeCons ->  UnifyM TypeCons
unifyEquals tCons = --trace' [MkSB tCons, MkSB tEnv] "Unify iteration" $ case (Set.minView tCons) of
                case Set.minView $ consEquals tCons of -- get a constraint from the set
                    Just (cons, consEquals') -> unifyEquals' cons (tCons { consEquals = consEquals'}) >>= unifyEquals
                    -- we're done, no constraints left in the set
                    Nothing -> return tCons
  where
    -- | Fully Unification (and Checking) for Equal rule
    unifyEquals' :: ConsEqual -> TypeCons -> UnifyM TypeCons

    -- two equal types - can by typevars, base units, unitVars, composites, anything that is fully equal
    -- unifyEquals' (ConsEqual x y) st | (x == y) = return st

    -- two equal ids - remove from set and ignore
    unifyEquals' (ConsEqual (E.TVar xId) (E.TVar yId)) tCons
       | (xId == yId) = return tCons

    -- replace all x with y
    unifyEquals' (ConsEqual x@(E.TVar xId) y) tCons
        | not (occursCheck x y) = do
            tEnv <- S.get
            -- We have to check the TypeEnv here to see if the TypeVar has already been subsituted, this is because,
            -- even though we run the subAddMap/subStack functions that will update the map and set, we may still have
            -- multiple substitutions for a tVar due to case-analysis on the current rule that may be a composite case
            -- i.e. TTArr or TTuple, hence the Equality rules within them would not have been updated
            case Map.lookup xId tEnv of
                 -- the tvar has already been updated at some point within this
                 -- ConsEqual rule, use this and recheck, reinsert a new equality constraint
                Just xT -> return $ tCons { consEquals = Set.insert (ConsEqual xT y) (consEquals tCons)}
                --Just xT -> uCon (xT, y) (tCons, tEnv) -- can also, but neater to reinsert into the tCons
                -- tvar doesn't exist, add and sub
                Nothing -> S.modify (\tEnv -> subAddMap x y tEnv) >> return (subStack x y tCons)

    -- replace all y with x
    unifyEquals' (ConsEqual x y@(E.TVar _)) st = unifyEquals' (ConsEqual y x) st
    --        | not (occursCheck y x) = return (subStack y x tCons, subMap y x tEnv)

    unifyEquals' (ConsEqual (E.TArr x1 x2) (E.TArr y1 y2)) st = do
        st' <- unifyEquals' (ConsEqual x1 y1) st
        unifyEquals' (ConsEqual x2 y2) st'

    unifyEquals' (ConsEqual (E.TTuple xs) (E.TTuple ys)) st | (length xs == length ys) =
        DF.foldlM (\st (x, y) -> unifyEquals' (ConsEqual x y) st) st (zip xs ys)


    -- Explicit UnitVar equality handling
    -- uV = uV
    unifyEquals' (ConsEqual (E.TFloat (U.UnitVar uV1)) (E.TFloat u2@(U.UnitVar uV2))) st | (uV1 == uV2) = return st
    -- uV = u
    unifyEquals' (ConsEqual t1@(E.TFloat (U.UnitVar uV1)) t2@(E.TFloat u2@(U.UnitC _))) st = do
        -- no occursCheck needed, no composite types
        -- simply add to tEnv and replace in tCons
        tEnv <- S.get
        case Map.lookup uV1 tEnv of
            -- the tvar has already been updated at some point within this
            -- ConsEqual rule, use this and recheck, reinsert a new equality constraint
            Just t1' -> return $ tCons { consEquals = Set.insert (ConsEqual t1' t2) (consEquals tCons)}
            --Just xT -> uCon (xT, y) (tCons, tEnv) -- can also, but neater to reinsert into the tCons
            -- tvar doesn't exist, add and sub
            Nothing -> S.modify (\tEnv -> subAddMap t1 t2 tEnv) >> return (subStack t1 t2 tCons)
    -- u = uV
    unifyEquals' (ConsEqual t1@(E.TFloat u2@(U.UnitC _)) t2@(E.TFloat (U.UnitVar uV1))) st = unifyEquals' (ConsEqual t2 t1) st

    -- two equal types - can by typevars, base units, unitVars, composites, anything that is fully equal
    -- TODO - enable at top of cases
    unifyEquals' (ConsEqual x y) st | (x == y) = return st
    -- uCon (x, y, uRule) st = undefined -- are these needed?

    -- can't unify types
    unifyEquals' (ConsEqual x y) st = trace' [MkSB x, MkSB y, MkSB st] "Type Error" $ throwError (printf "(TC01) - cannot unify %s and %s" (show x) (show y))


type ConsSumS = Set.Set ConsSum

-- | (Some Unification) and Checking for Sum rule
unifySum :: TypeCons ->  UnifyM TypeCons
unifySum tCons = do
    _ <- unifySum' (consSums tCons) Set.empty
    return tCons

  where
    -- need to keep looping until no-more changes occur
    unifySum' :: ConsSumS -> ConsSumS -> UnifyM ()
    unifySum' curSums newSums = case Set.minView curSums of
        Just (consSum, curSums') -> processSum consSum curSums' newSums >>= uncurry unifySum' -- continus loop
        -- set empty, check new set if something more to iterate on, else quit
        Nothing | (Set.size newSums < Set.size curSums) -> unifySum' newSums Set.empty -- somthing changed, loop again
        Nothing | otherwise -> return () -- no change, have infered all we could

    -- actually process the sum rule
    -- will always be a FLoat val
    processSum :: ConsSum -> ConsSumS -> ConsSumS -> UnifyM (ConsSumS, ConsSumS)

    -- 3 uVars - can do fuck all, copy to newSums
    processSum cons@(ConsSum (E.TFloat (U.UnitVar _)) (E.TFloat (U.UnitVar _)) (E.TFloat (U.UnitVar _))) curSums newSums = return (curSums, Set.insert cons newSums)

    -- 2 unit vars - can only do something if have a NoUnit, otherwise fuck all
    -- uV uV NU
    processSum (ConsSum u1@(E.TFloat (U.UnitVar _)) u2@(E.TFloat (U.UnitVar _)) (E.TFloat U.NoUnit)) curSums newSums = replaceUnit u1 u2 curSums newSums
    -- uV NU uV
    processSum (ConsSum u1@(E.TFloat (U.UnitVar _)) (E.TFloat U.NoUnit) u2@(E.TFloat (U.UnitVar _))) curSums newSums = replaceUnit u1 u2 curSums newSums
    -- NU uV uV
    processSum (ConsSum (E.TFloat U.NoUnit) u1@(E.TFloat (U.UnitVar _)) u2@(E.TFloat (U.UnitVar _))) curSums newSums = replaceUnit u1 u2 curSums newSums

    -- 1 unit var -- can infer and replace the correct unit
    -- u u uV
    processSum (ConsSum (E.TFloat u1@(U.UnitC _)) (E.TFloat u2@(U.UnitC _)) x@(E.TFloat (U.UnitVar _))) curSums newSums = replaceUnit x (E.TFloat $ U.addUnit u1 u2) curSums newSums
    -- u uV u
    processSum (ConsSum (E.TFloat u1@(U.UnitC _)) x@(E.TFloat (U.UnitVar _)) (E.TFloat u3@(U.UnitC _))) curSums newSums = replaceUnit x (E.TFloat $ U.subUnit u3 u1) curSums newSums
    -- uV u u
    processSum (ConsSum x@(E.TFloat (U.UnitVar _)) (E.TFloat u2@(U.UnitC _))  (E.TFloat u3@(U.UnitC _))) curSums newSums = replaceUnit x (E.TFloat $ U.subUnit u3 u2) curSums newSums

    processSum cons curSums newSums = errorDump [MkSB cons, MkSB curSums, MkSB newSums] "Found an invalid ConsSum"

    -- update all units x->y
    -- where x = Float (UnitVar uID), y = Float Unit
    -- TODO - this may be in error, switch to units
    replaceUnit :: E.Type -> E.Type -> ConsSumS -> ConsSumS -> UnifyM (ConsSumS, ConsSumS)
    replaceUnit x y tCons
        | not (occursCheck x y) = do
                -- unlike the unifyEqual rule, we do not need to check the TypeMap to see if uVar already exists, as we
                -- already substitute the map/tCons and there are not compositite cases where this subsitituion would not be sufficcent
                -- just add and sub
                S.modify (\tEnv -> subAddMap x y tEnv)
                return $ subStack x y tCons
        | otherwise = return tCons

-- TODO - other rules here
-- uCon ConsSameDim -- check UC convCast rul
-- | (Minimal unification) and Checking for SameDim rule
unifySameDim :: SysS.UnitsState -> TypeCons ->  UnifyM TypeCons
unifySameDim uState tCons = undefined
  where
    processSameDim :: ConsSameDim -> UnifyM ()

    -- both same uVar - ignore
    processSameDim (ConsSameDim u1@(E.TFloat (U.UnitVar uV1)) u2@(E.TFloat (U.UnitVar uV2))) = return ()

    -- 2 uVars - do fuck all
    processSameDim (ConsSameDim u1@(E.TFloat (U.UnitVar _)) u2@(E.TFloat (U.UnitVar _))) = return ()

    -- 1 uVar - can only do something if have a NoUnit, have to sub in
    processSameDim (ConsSameDim u1@(E.TFloat (U.UnitVar _)) u2@(E.TFloat U.NoUnit)) = return ()
    processSameDim (ConsSameDim u1@(E.TFloat U.NoUnit) u2@(E.TFloat (U.UnitVar _))) = return ()

    -- 1 uVar - other is a unit, fuck all
    processSameDim (ConsSameDim u1@(E.TFloat (U.UnitVar _)) u2@(E.TFloat (U.UnitC _))) = return ()
    processSameDim (ConsSameDim u1@(E.TFloat (U.UnitC _)) u2@(E.TFloat (U.UnitVar _))) = return ()

    -- both units or nounits - check they are equal dimension
    processSameDim (ConsSameDim t1@(E.TFloat U.NoUnit) t2@(E.TFloat U.NoUnit)) = return ()


    processSameDim (ConsSameDim t1@(E.TFloat u1@(U.UnitC _)) t2@(E.TFloat u2@(U.UnitC _))) = do
        -- make sure is within same dim as u, and a conversion path exists
        lift $ U.calcConvExpr u1 u2 (L.get SysS.lUnitDimEnv uState) (L.get SysS.lConvEnv uState)
        return ()





