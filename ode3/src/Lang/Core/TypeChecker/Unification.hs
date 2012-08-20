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
import Utils.CommonImports
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap

import qualified SysState as SysS
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

-- | try to update all occurances of (tVar x) within tCons using current tVMaps/uVMaps
updateStack :: TypeCons -> UnifyM TypeCons
updateStack tCons = do
    (tVEnv, uVEnv) <- S.get
    updateStack' tVEnv uVEnv
  where
    updateStack' tVEnv uVEnv = return $ tCons { conTypeS = conTypeS', conUnitS = conUnitS' }
      where
        conTypeS' = Set.map updateConType $ conTypeS tCons
        updateConType (ConEqual a b) = ConEqual (updateTypes a) (updateTypes b)

        conUnitS' = Set.map updateConUnit $ conUnitS tCons
        updateConUnit (ConSum a b c) = ConSum (updateUnits a) (updateUnits b) (updateUnits c)
        updateConUnit (ConSameDim a b) = ConSameDim (updateUnits  a) (updateUnits b)

        updateTypes = E.mapType (\t -> case t of
            t'@(E.TVar tV) -> Map.findWithDefault t' tV tVEnv
            t'@(E.TFloat (U.UnitVar uV)) -> maybe t' (E.TFloat) $ Map.lookup uV uVEnv
            _ -> t)

        updateUnits u@(U.UnitVar uV) = Map.findWithDefault u uV uVEnv
        updateUnits u = u

-- | replace all occurances of t1->t2 in the set of constraints
subEqualS :: E.Type -> E.Type -> ConTypeS -> ConTypeS
subEqualS t1 t2 = Set.map subEqual
  where
    subEqual (ConEqual a b) = ConEqual (subTTerm t1 t2 a) (subTTerm t1 t2 b)

-- | replace all occurances of u1->u2 in the set of constraints
subUnitS :: U.Unit -> U.Unit -> ConUnitS -> ConUnitS
subUnitS u1 u2 = Set.map subUnit
  where
    subUnit (ConSum a b c) = ConSum (subUTerm u1 u2 a) (subUTerm u1 u2 b) (subUTerm u1 u2 c)
    subUnit (ConSameDim a b) = ConSameDim (subUTerm u1 u2 a) (subUTerm u1 u2 b)

-- | replaces all occurances of (tVar x) with y in tEnv, then add [x->y] to the tEnv
-- TODO - prob should have a separate env for UnitVar Ids -> Units/Types
subAddType t1 t2 tEnv = case t1 of
    E.TVar tV1 -> Map.insert tV1 t2 tEnv'
    _ -> tEnv'
  where
    tEnv' = Map.map (subTTerm t1 t2) tEnv

-- | replaces all occurances of t1 with t2 in the tTerm t
subTTerm t1 t2 t@(E.TTuple ts)
    | t == t1 = t2
    | otherwise = E.TTuple $ map (subTTerm t1 t2) ts
subTTerm t1 t2 t@(E.TRecord ts)
    | t == t1 = t2
    | otherwise = E.TRecord $ Map.map (subTTerm t1 t2) ts
subTTerm t1 t2 t@(E.TArr fromT toT)
    | t == t1 = t2
    | otherwise = E.TArr (subTTerm t1 t2 fromT) (subTTerm t1 t2 toT)
subTTerm t1 t2 t@(E.TTypeCons tName tUnwrap)
    | t == t1 = t2
    | otherwise = E.TTypeCons tName $ subTTerm t1 t2 tUnwrap
subTTerm t1 t2 t = if t == t1 then t2 else t

-- | checks that tVar x does not exist in tTerm t, stop recursive substitions
occursCheck :: E.Type -> E.Type -> Bool
occursCheck t1 t@(E.TTuple ts)
    | t == t1 = True
    | otherwise = any (occursCheck t1) ts
occursCheck t1 t@(E.TRecord ts)
    | t == t1 = True
    | otherwise = DF.any (occursCheck t1) ts
occursCheck t1 t@(E.TArr fromT toT)
    | t == t1 = True
    | otherwise = (occursCheck t1 fromT) || (occursCheck t1 toT)
occursCheck t1 t@(E.TTypeCons tName tUnwrap)
    | t == t1 = True
    | otherwise = occursCheck t1 tUnwrap
occursCheck t1 t = if t == t1 then True else False

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
subUTerm u1 u2 u = if u1 == u then u2 else u

unitOccursCheck :: U.Unit -> U.Unit -> Bool
unitOccursCheck = (==)

isUnitVar :: U.Unit -> Bool
isUnitVar (U.UnitVar _) = True
isUnitVar _ = False

-- Main functions ------------------------------------------------------------------------------------------------------

-- | unify takes a set of type contraints and attempts to unify all types, inc TVars
-- based on HM - standard constraint unification algorithm
-- Do we want to return the set of remaining constraints as well??
unify :: SysS.UnitsState -> TypeCons -> MExcept (TypeVarEnv, UnitVarEnv)
unify uState tCons = snd <$> S.runStateT unifyM (Map.empty, Map.empty)
  where
    --trace' [MkSB tCons] "Initial Unify tCons" $
    unifyM = do
        -- trace' [MkSB tCons] "Start type unify" $ return ()
        conTypeS' <- unifyTypes (conTypeS tCons)
        -- return set should be empty
        unless (Set.null conTypeS') $ errorDump [MkSB conTypeS'] "Unification Error - set not empty" assert

        tCons' <- updateStack (tCons { conTypeS = conTypeS' })
        unifyUnitsLoop (conUnitS tCons')

    -- need to repeat unifyUnits until we can infer no more from tCons
    unifyUnitsLoop conUnitS = do
        -- trace' [MkSB conUnitS] "Start units unify loop" $ return ()
        conUnitS' <- unifyUnits uState conUnitS
        if (conUnitS /= conUnitS') then unifyUnitsLoop conUnitS'
            else do
                -- trace' [MkSB conUnitS'] "Finish units unify loop" $ return ()
                return conUnitS'


-- | Unification (Full) and Checking (Some) for Equal rule
unifyTypes :: ConTypeS ->  UnifyM ConTypeS
unifyTypes conTypeS = unifyTypesLoop conTypeS
  where
    unifyTypesLoop curS = case Set.minView curS of
        Just (con, curS') -> processType con curS' >>= unifyTypesLoop      -- get a constraint from the set if poss
        Nothing -> return curS                                           -- we're done, no constraints left in the set

    processType :: ConType -> ConTypeS -> UnifyM ConTypeS
    -- two equal types - can be typevars, base units, unitVars, composites, anything that is fully equal
    processType (ConEqual t1 t2) curS | (t1 == t2) = return curS

    -- TypeVars
    -- tV = t, replace all x with y
    processType (ConEqual t1@(E.TVar tV1) t2) curS | not (occursCheck t1 t2) = do
        (tVEnv, _) <- S.get
        -- We have to check the TypeEnv here to see if the TypeVar has already been subsituted, this is because,
        -- even though we run the subAddMap/subStack functions that will update the map and set, we may still have
        -- multiple substitutions for a tVar due to case-analysis on the current rule that may be a composite case
        -- i.e. TTArr or TTuple, hence the Equality rules within them would not have been updated
        case Map.lookup tV1 tVEnv of
             -- the tvar has already been updated at some point within this
             -- ConEqual rule, use this and recheck, reinsert a new equality constraint
            Just t1' -> return $ Set.insert (ConEqual t1' t2) curS
            --Just t1' -> processType (t1', t2) (tCons, tEnv) -- can also call direct, but neater to reinsert into the tCons
            -- tvar doesn't exist, add to tVEnv and sub the type
            Nothing -> S.modify (\(tVEnv, uVEnv) -> (subAddType t1 t2 tVEnv, uVEnv)) >> (return $ subEqualS t1 t2 curS)

    -- t = tV, replace all y with x
    processType (ConEqual t1 t2@(E.TVar _)) curS = processType (ConEqual t2 t1) curS

    -- Composite, Functions
    processType (ConEqual (E.TArr t1From t1To) (E.TArr t2From t2To)) curS =
        processType (ConEqual t1From t2From) curS >>= processType (ConEqual t1To t2To)

    -- Composite, Newtypes
    processType (ConEqual t1@(E.TTypeCons t1Name t1Unwrap) t2@(E.TTypeCons t2Name t2Unwrap)) curS =
        trace' [MkSB t1, MkSB t2] "Unify TypeCons" $ processType (ConEqual t1Unwrap t2Unwrap) curS

    -- Composite, Tuples
    processType (ConEqual (E.TTuple t1s) (E.TTuple t2s)) curS | (length t1s == length t2s) =
        DF.foldlM (\curS (t1, t2) -> processType (ConEqual t1 t2) curS) curS (zip t1s t2s)

    -- Composite, Records
    -- check ids are equal (not subtype/subset), then combine using ids and run equalty on each pair
    processType (ConEqual (E.TRecord t1s) (E.TRecord t2s)) curS | recordIdsEq =
        DF.foldlM (\curS (t1, t2) -> processType (ConEqual t1 t2) curS) curS (Map.intersectionWith (,) t1s t2s)
      where
        -- subtyping equality check for records Ids, t1 < t2
        recordIdsEq = all (\refId -> Map.member refId t2s) (Map.keys t1s)
        -- recordIdsEq = (Map.keys t1s == Map.keys t2s) -- full equality check

    -- Composite, Tuple<->Record (should only be used internaly, as drops labels from record)
--    processType (ConEqual t1@(E.TTuple t1s) (E.TRecord t2s)) curS = processType (ConEqual t1 (E.TTuple $ E.dropLabels t2s)) curS
--    processType (ConEqual t1@(E.TRecord _) t2@(E.TTuple _)) curS = processType (ConEqual t2 t1) curS

    -- UnitVars equality handling
--    -- uV = uV
--    processType (ConEqual (E.TFloat (U.UnitVar uV1)) (E.TFloat u2@(U.UnitVar uV2))) st | (uV1 == uV2) = return st
    -- uV = u
    processType (ConEqual t1@(E.TFloat u1@(U.UnitVar uV1)) t2@(E.TFloat u2)) curS = do
        -- no occursCheck needed, no composite types
        -- simply add to tEnv and replace in tCons
        (_, uVEnv) <- S.get
        case Map.lookup uV1 uVEnv of
            -- the uVar has already been updated at some point within this
            -- ConEqual rule, use this and recheck, reinsert a new equality constraint
            Just u1' -> return $ Set.insert (ConEqual (E.TFloat u1') t2) curS
            -- uVar doesn't exist, add to both maps, and sub
            Nothing -> S.modify (\(tVEnv, uVEnv) -> (subAddType t1 t2 tVEnv, subAddUnit u1 u2 uVEnv)) >> return (subEqualS t1 t2 curS)

    -- u = uV
    processType (ConEqual t1@(E.TFloat _) t2@(E.TFloat (U.UnitVar _))) curS = processType (ConEqual t2 t1) curS

    -- can't unify types
    processType (ConEqual t1 t2) curS = trace' [MkSB t1, MkSB t2, MkSB curS] "Type Error" $ throwError (printf "(TC01) - cannot unify %s and %s" (show t1) (show t2))



unifyUnits :: SysS.UnitsState -> ConUnitS ->  UnifyM ConUnitS
unifyUnits uState conUnitS = unifyUnitLoop conUnitS
  where
    unifyUnitLoop :: ConUnitS -> UnifyM ConUnitS
    unifyUnitLoop startS = unifyUnitLoop' (startS, Set.empty)
      where
        -- need to keep looping until no-more changes occur
        unifyUnitLoop' :: (ConUnitS, ConUnitS) -> UnifyM ConUnitS
        unifyUnitLoop' (curS, newS) = case Set.minView curS of
            Just (con, curS') -> processUnit con (curS', newS) >>= unifyUnitLoop' -- continue loop
            -- set empty, check new set if something more to iterate on, else quit
            -- TODO - check we are comparing the right sets here
            Nothing | (newS /= startS) -> unifyUnitLoop newS -- somthing changed, restart loop again from top
            Nothing | otherwise -> return newS -- no change, have infered all we could, return the remaining

    -- | Unification (Some) and Checking (Full) for Sum rule

    -- actually process the sum rule
    -- We ignore several cases, mnainly when two uV are equal (uV1 == uv2), in which case we can infer slightly more,
    -- however we assume that such uV will eventually be infered, and thus the other rules will then come into play
    -- need to test if true. Hoever we do add some NoUnit rules
    processUnit :: ConUnit -> (ConUnitS, ConUnitS) -> UnifyM (ConUnitS, ConUnitS)
    -- 3 uVars - can do fuck all, copy to newSums
    processUnit con@(ConSum (U.UnitVar _) (U.UnitVar _) (U.UnitVar _)) (curS, newS) = return (curS, Set.insert con newS)

    -- 2 uVars - can only do something if have a NoUnit, otherwise fuck all
    -- uV uV NU - uV1 and uV2 must be NoUnits too
    processUnit (ConSum u1@(U.UnitVar _) u2@(U.UnitVar _) U.NoUnit) st =
        replaceUnit u1 U.NoUnit st >>= replaceUnit u2 U.NoUnit
    -- uV NU uV - both uVs must be equal
    processUnit (ConSum u1@(U.UnitVar _) U.NoUnit u3@(U.UnitVar _)) st = replaceUnit u1 u3 st
    -- NU uV uV - both uVs must be equal
    processUnit (ConSum U.NoUnit u2@(U.UnitVar _) u3@(U.UnitVar _)) st = replaceUnit u2 u3 st

    -- 1 uVar -- can infer and replace the correct unit
    -- u/NU u/NU uV
    processUnit (ConSum u1 u2 u3@(U.UnitVar _)) st | not (isUnitVar u1) && not (isUnitVar u2) = replaceUnit u3 (U.addUnit u1 u2) st
    -- u/NU uV u/NU
    processUnit (ConSum u1 u2@(U.UnitVar _) u3) st | not (isUnitVar u1) && not (isUnitVar u3) = replaceUnit u2 (U.subUnit u3 u1) st
    -- uV u/NU u/NU
    processUnit (ConSum u1@(U.UnitVar _) u2 u3) st | not (isUnitVar u2) && not (isUnitVar u3) = replaceUnit u1 (U.subUnit u3 u2) st

    -- 0 uVars - simply check the sums are correct
    -- u/NU u/NU u/NU
    processUnit con@(ConSum u1 u2 u3) st | not (isUnitVar u1) && not (isUnitVar u2) && not (isUnitVar u3) =
        trace' [MkSB con, MkSB st] "ConSum" $ if U.addUnit u1 u2 == u3 then return st
                                else throwError $ printf "Invalid unit calculation - %s + %s does not equal %s" (show u1) (show u2) (show u3)

    -- Unification (Minimal) and Checking (Full) for SameDim rule ------------------------------------------------------
    -- completely equal - hence must be same dimension
    processUnit (ConSameDim u1 u2) st | u1 == u2 = return st

    -- both same uVar - ignore
    -- processSameDim (ConSameDim u1@(U.UnitVar uV1) u2@(U.UnitVar uV2)) st | uV1 == uV2 = return st

    -- 2 uVars - can do fuck all yet so keep it, in contraqst to unifyEquals where we substitute, here need to keep the original uVs
    processUnit con@(ConSameDim u1@(U.UnitVar _) u2@(U.UnitVar _)) (curS, newS) = return (curS, Set.insert con newS)

    -- 1 uVar - can infer in case of NoUnit, then uV must also be NoUnit
    -- TODO - check, does this ever occur?
    -- Uv NU
    processUnit (ConSameDim u1@(U.UnitVar _) U.NoUnit) st = replaceUnit u1 U.NoUnit st
    processUnit (ConSameDim U.NoUnit u2@(U.UnitVar _)) st = replaceUnit u2 U.NoUnit st

    -- 1 uVar - other is a unit, fuck all
    -- Uv u
--    processUnit (ConSameDim u1@(U.UnitVar _) u2@(U.UnitC _)) st = return st
--    processUnit (ConSameDim u1@(U.UnitC _) u2@(U.UnitVar _)) st = return st

    -- both units or nounits - check they are equal dimension
    processUnit (ConSameDim u1 u2) st | not (isUnitVar u1) && not (isUnitVar u2) = do
        -- make sure is within same dim as u, and a conversion path exists
        -- lift $ U.convertCastUnit u1 u2 (L.get SysS.lUnitDimEnv uState) (L.get SysS.lConvEnv uState)
        lift $ U.unitsSameDim u1 u2 (L.get SysS.lUnitDimEnv uState)
        return st

    -- anything else - pass on to the newS for next iteration
    processUnit con (curS, newS) = return (curS, Set.insert con newS)
        -- errorDump [MkSB con, MkSB curS, MkSB newS] "Found an invalid ConSameDim state"

    -- update all units x->y
    -- where x = Float (UnitVar uID), y = Float Unit
    replaceUnit :: U.Unit -> U.Unit -> (ConUnitS, ConUnitS) -> UnifyM (ConUnitS, ConUnitS)
    replaceUnit u1 u2 (curS, newS)
        | not (unitOccursCheck u1 u2) = do
                -- unlike the unifyEqual rule, we do not need to check the TypeMap to see if uVar already exists, as we
                -- already substitute the map/tCons and there are not compositite cases where this subsitituion would not be sufficcent
                -- just add and sub
                S.modify (\(tEnv, uEnv) -> (tEnv, subAddUnit u1 u2 uEnv))
                let curS' = subUnitS u1 u2 curS
                let newS' = subUnitS u1 u2 newS
                return (curS', newS')
        | otherwise = return (curS, newS)

