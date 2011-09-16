-----------------------------------------------------------------------------
--
-- Module      :  Core.Type2
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | New type-checker that uses full HM algorithm - hopefully to be extedned to support higher-order
-- differntials and units
--
-----------------------------------------------------------------------------

module Core.Type2 (
typeCheck
) where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Text.Printf (printf)
import qualified Core.AST as C
import Utils.Utils
import Utils.MonadSupply


-- | Types
data Type =    -- TUnknown -- TODO - remove
                TVar Int
                | TBool
                | TFloat
                | TArr Type Type
                | TTuple [Type] -- don't want to allow tuples of tuples
                deriving (Show, Eq, Ord)

--type Type = Primitve TType | Typevar Int

typeCheck :: C.OrdModel Int -> MExcept (C.OrdModel Int)
typeCheck cModel = trace (show tCons) (trace (show tCons') (return cModel))
--    checkedTypeMap <- check cModel unifiedTypeMap
--    return $ typeModel cModel checkedTypeMap
  where
    tCons = constrain cModel
    tCons' = unify tCons

-- do we store within the typeEnv a type for each individual variable or for each unified bindings?
-- let's try both?
type TypeEnv = Map.Map Int Type
type TypeCons = Set.Set (Type, Type)
type TypeConsM = SupplyT Int (State TypeCons)

addConstraint :: Type -> Type -> TypeConsM ()
addConstraint t1 t2 = do
    tS <- lift get
    let tS' = Set.insert (t1, t2) tS
    lift $ put tS'

newTypevar :: TypeConsM Type
newTypevar = liftM TVar supply

--consBind :: C.Bind Int -> Type -> TypeEnv -> TypeEnv
--consBind b t tEnv = Map.insert b t tEnv
--consBind (C.MultiBind bs) (TTuple ts) tEnv = foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv (zip bs ts)

--consBind (C.SingleBind b) t tEnv = Map.insert b t tEnv
--consBind (C.MultiBind bs) (TTuple ts) tEnv = foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv (zip bs ts)
-- this shouldn't happen
--consBind (C.MultiBind bs) t tEnv = trace ("shit\n" ++ (show bs)) (trace (show t) (trace (show tEnv) undefined))

-- | Adds a set of constraints for linking a multibind to a TVar
multiBindConstraint :: C.Bind Int -> Type -> TypeEnv -> TypeConsM TypeEnv
multiBindConstraint (C.LetBind bs) (TVar v) tEnv = do
    -- create the new tvars for each binding
    bTs <- mapM (\_ -> newTypevar) bs
    -- add the constaint
    addConstraint (TTuple bTs) (TVar v)
    -- add the tvars to the type map
    DF.foldlM (\tEnv (b, bT) -> return $ Map.insert b bT tEnv) tEnv (zip bs bTs)

constrain :: C.OrdModel Int -> TypeCons
constrain cModel = trace (show tEnv) consSet
  where
    (tEnv, consSet) = runState (evalSupplyT consM [1..]) (Set.empty)

    consM :: TypeConsM TypeEnv
    consM = DF.foldlM consTop Map.empty (C.getOrdSeq cModel)

--    consTop tEnv (C.TopLet (C.SingleBind b) e) = do
--        (eT, tEnv') <- consExpr tEnv e
--        -- extend and return tEnv
--        return $ Map.insert b eT tEnv'

    consTop tEnv (C.TopLet (C.LetBind bs) e) = do
        (eT, tEnv') <- consExpr tEnv e
        -- extend and return tEnv
        -- return $ Map.insert i eT tEnv'
        case eT of
            (TTuple ts) -> return $ foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv' (zip bs ts)
            (TVar v) | (length bs > 1) -> multiBindConstraint (C.LetBind bs) (TVar v) tEnv'
            t | length bs == 1 -> return $ Map.insert (head bs) eT tEnv'
            _ -> trace ("toplet shit\n" ++ (show bs)) (trace (show eT) (trace (show tEnv') undefined))

    consTop tEnv (C.TopAbs (C.AbsBind b) arg e) = do
        fromT <- newTypevar
        -- extend the tEnv
        let tEnv' = Map.insert arg fromT tEnv
        (toT, tEnv'') <- consExpr tEnv' e
        -- add a constraint?

        -- extend and return tEnv
        return $ Map.insert b (TArr fromT toT) tEnv''


    -- | map over the expression elements, creating constraints as needed,
    -- TODO - do we need to uniquely refer to each expression within AST?, or just bindings?
    consExpr tEnv (C.Var v) = return $ (tEnv Map.! v, tEnv)

    consExpr tEnv (C.Lit l) = return $ (getLitType l, tEnv)

    consExpr tEnv (C.App f e) = do
        -- as HOFs not allowed
        -- fT =  consExpr tEnv f
        let fT = tEnv Map.! f
        (eT, tEnv') <- consExpr tEnv e
        toT <- newTypevar
        -- add constraint
        addConstraint fT (TArr eT toT)
        return (toT, tEnv')

    -- TODO - do we need to return the new tEnv here?
--    consExpr tEnv (C.Let (C.SingleBind b) e1 e2) = do
--        (e1T, tEnv') <- consExpr tEnv e1
--        -- extend tEnv with new env
--        let tEnv'' = Map.insert b e1T tEnv'
--        consExpr tEnv'' e2
--        -- add constraint ?
--        -- return e2T

    consExpr tEnv (C.Let (C.LetBind bs) e1 e2) = do
        (e1T, tEnv') <- consExpr tEnv e1
        -- extend tEnv with new env
        tEnv'' <- case e1T of
            (TTuple ts) -> return $ foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv' (zip bs ts)
            (TVar v) | (length bs > 1) -> multiBindConstraint (C.LetBind bs) (TVar v) tEnv'
            t | length bs == 1 -> return $ Map.insert (head bs) e1T tEnv'
            _ -> trace ("let shit\n" ++ (show bs)) (trace (show e1T) (trace (show tEnv') undefined))
        consExpr tEnv'' e2

    consExpr tEnv (C.Op op e) = do
        let (TArr fromT toT) = getOpType op
        (eT, tEnv') <- consExpr tEnv e
        -- NOTE - we don't need to gen a new tvar here as the totype is always fixed so the toT will always unify to it
        addConstraint fromT eT
        return (toT, tEnv')

    consExpr tEnv (C.If eB eT eF) = do
        (eBT, tEnv') <- consExpr tEnv eB
        addConstraint eBT TBool
        (eTT, tEnv'') <- consExpr tEnv' eT
        (eFT, tEnv''') <- consExpr tEnv'' eF
        addConstraint eTT eFT
        return (eFT, tEnv''')

    consExpr tEnv (C.Tuple es) = liftM consTuple (DF.foldlM consElem ([], tEnv) es)
      where
        consElem (eTs, tEnv) e = consExpr tEnv e >>= (\(eT, tEnv') -> return (eT:eTs, tEnv'))
        consTuple (eTs, tEnv) = (TTuple (reverse eTs), tEnv)

    -- other exprs
    consExpr tEnv _ = undefined --return (TFloat, tEnv)


getLitType :: C.Literal -> Type
getLitType l = case l of
    C.Boolean _ -> TBool
    C.Num _ -> TFloat
    C.NumSeq _ -> TFloat


-- TODO - should this be moved into the AST?
-- | Takes an operator and returns the static type of the function
getOpType :: C.Op -> Type
getOpType op = case op of
    C.Add -> binNum
    C.Sub -> binNum
    C.Mul -> binNum
    C.Div -> binNum
    C.Mod -> binNum
    C.LT -> binRel
    C.LE -> binRel
    C.GT -> binRel
    C.GE -> binRel
    C.EQ -> binRel
    C.NEQ -> binRel
    C.And -> binLog
    C.Or -> binLog
    C.Not -> TArr TBool TBool
    --C.Unpack i -> C.TArr (C.TTuple (createUnpackFrom i)) inType -- TODO - is this right?
    -- C.Nop -> C.TUnknown -- why is this here??!
  where
    binNum = TArr (TTuple [TFloat, TFloat]) TFloat
    binRel = TArr (TTuple [TFloat, TFloat]) TBool
    binLog = TArr (TTuple [TBool, TBool]) TBool
    --createUnpackFrom i = hd ++ (inType:tl)
    --  where
    --    (hd, tl) = splitAt i . replicate i $ C.TUnknown


unify :: TypeCons -> TypeEnv
unify tCons = snd $ u (tCons, Map.empty)
  where
    u (tCons, tMap) = case (Set.minView tCons) of
                        Just (constraint, tCons') -> u (uCon constraint (tCons', tMap))
                        Nothing -> (tCons, tMap)

    uCon :: (Type, Type) -> (TypeCons, TypeEnv) -> (TypeCons, TypeEnv)
    -- two equal ids - remove from set and ignore
    uCon (TVar xId, TVar yId) st
       | (xId == yId) = st

    uCon (x, y) st
       | (x == y) = st

    -- replace all x with y
    uCon (x@(TVar _), y) (tCons, tMap)
        | not (occursCheck x y) = (subStack x y tCons, subMap x y tMap)

    -- replace all y with x
    uCon (x, y@(TVar _)) (tCons, tMap)
        | not (occursCheck y x) = (subStack y x tCons, subMap y x tMap)

    -- composite types
    uCon (TArr x1 x2, TArr y1 y2) st = uCon (x2, y2) (uCon (x1, y1) st)

    uCon (TTuple xs, TTuple ys) st = foldl (\st (x, y) -> uCon (x, y) st) st (zip xs ys)

    uCon (x, y) st = trace ("type error\n" ++ show (x, y, st)) undefined -- throwError here!

    -- replaces all occurances of tVar x with y in tCons
    subStack x y tCons = Set.map subTCon tCons
      where
        subTCon (a, b) = (subTTerm x y a, subTTerm x y b)

    -- replaces all occurances of tVar x with y in tMap
    subMap x@(TVar xId) y tMap = Map.insert xId y tMap'
      where
        tMap' = Map.map (subTTerm x y) tMap

    -- checks that tVar x does not exist in tTerm t, stop recursive substitions
    occursCheck x t@(TTuple ts)
        | t == x = True
        | otherwise = any (occursCheck x) ts
    occursCheck x t@(TArr fromT toT)
        | t == x = True
        | otherwise = (occursCheck x fromT) || (occursCheck x toT)
    occursCheck x t = if t == x then True else False

    -- replaces all occurances of x with y in the tTerm t
    subTTerm x y t@(TTuple ts)
        | t == x = y
        | otherwise = TTuple $ map (subTTerm x y) ts
    subTTerm x y t@(TArr fromT toT)
        | t == x = y
        | otherwise = TArr (subTTerm x y fromT) (subTTerm x y toT)
    subTTerm x y t = if t == x then y else t

