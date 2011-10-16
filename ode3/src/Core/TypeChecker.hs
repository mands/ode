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
{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

module Core.TypeChecker (
typeCheck, typeCheckApp, TypeVarEnv, TypeCons, unify
) where

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
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
import qualified Utils.OrdMap as OrdMap

type TypeEnv    = C.TypeMap
type TypeVarEnv = Map.Map C.Id C.Type
-- type env for modules, only one needed as vars should be unique, holds a type var for the first occurance of a module var
type ModTypeEnv = Map.Map (C.VarId C.Id) C.Type
type TypeCons   = Set.Set (C.Type, C.Type)
type TypeConsM  = SupplyT Int (State TypeCons)


-- TODO - un-Do this!
typeCheck :: C.Module C.Id -> MExcept (C.Module C.Id)
typeCheck mod@(C.LitMod exprMap modData) = do
    let ((tEnv, _), tCons) = constrain exprMap
    -- unify the types and get the new typemap
    tVarMap <- unify tCons
    -- substitute to obtain the new type env
    tEnv' <- subTVars tEnv tVarMap False
    let modData' = updateModData modData tEnv'

    return $ C.LitMod exprMap modData'

typeCheck mod@(C.FunctorMod args exprMap modData) = do
    let ((tEnv, mTEnv), tCons) = constrain exprMap
    -- unify the types and get the new typemap
    tVarMap <- unify tCons
    -- substitute to obtain the new type env
    tEnv' <- subTVars tEnv tVarMap True
    mTEnv' <- subTVars mTEnv tVarMap True
    let modData' = updateModData modData tEnv'
    let args' = createFunModArgs args mTEnv'
    return $ C.FunctorMod args' exprMap modData'
  where
    -- create the public module signatures for Functors
    createFunModArgs :: C.FunArgs -> ModTypeEnv -> C.FunArgs
    createFunModArgs args mTEnv = Map.foldrWithKey addArg args mTEnv
      where
        -- add the type for M.v into the args OrdMap
        addArg (C.ModVar m v) t args = OrdMap.update updateModArgs m args
          where
            updateModArgs modMap = Just (Map.insert v t modMap)

-- takes the funcModule, an closed enviroment of the module args,
typeCheckApp :: C.Module C.Id -> C.ModuleEnv ->  MExcept (C.Module C.Id, C.ModuleEnv)
typeCheckApp fMod@(C.FunctorMod funArgs _ _) modEnv = do
    -- get the complete typevar map for an application
    tCons <- DF.foldlM constrainSigs Set.empty (OrdMap.toList funArgs)
    tVarMap <- unify tCons
    fMod' <- updateMod tVarMap fMod
    modEnv' <- DT.mapM (updateMod tVarMap) modEnv
    return (fMod', modEnv')
  where
    -- take a single mod arg for the functor, compare the sig with the one for the id within the modEnv,
    -- add all sigs to the typeCons via a foldr
    constrainSigs :: TypeCons -> (C.SrcId, C.SigMap) -> MExcept TypeCons
    constrainSigs typeCons (funcArgId, funcArgSig) = DF.foldrM compareTypes typeCons (Map.toList funcArgSig)
      where
        -- the module referenced by the arg
        argMod@(C.LitMod _ argModData) = modEnv Map.! funcArgId
        -- comparing the types for each binding by adding to the typeconstraint set
        compareTypes (b,tFunc) typeCons = typeCons'
          where
            typeCons' = case (Map.lookup b (C.modSig argModData)) of
                Just tArg -> Right $ Set.insert (tFunc, tArg) typeCons
                Nothing -> throwError $ "(MO05) - Invalid functor signature, cannot find ref " ++ show b ++ " in module argument" ++ funcArgId

    -- update a module based on the new type information
    updateMod :: TypeEnv -> C.Module C.Id -> MExcept (C.Module C.Id)
    updateMod tVarMap (C.LitMod exprMap modData) = do
        tEnv' <- subTVars (C.modTMap modData) tVarMap False
        let modData' = updateModData modData tEnv'
        return $ C.LitMod exprMap modData'

    updateMod tVarMap (C.FunctorMod args exprMap modData) = do
        tEnv' <- subTVars (C.modTMap modData) tVarMap False
        let modData' = updateModData modData tEnv'
        -- create the new funArgs based on the new tVarMap
        args' <- DT.mapM (\idMap -> subTVars idMap tVarMap False) args
        return $ C.FunctorMod args' exprMap modData'


-- | Update the module data with the public module signature and internal typemap
-- we create the mod signature by mapping over the idbimap data and looking up each value from the internal typemap
updateModData :: C.ModuleData -> TypeEnv -> C.ModuleData
updateModData modData tEnv = modData { C.modTMap = tEnv, C.modSig = modSig }
  where
    idMap = Bimap.toMap (C.modIdBimap modData)
    modSig = Map.map (\id -> tEnv Map.! id) idMap


-- | use the TVar map to undate a type enviroment and substitute all TVars
subTVars :: Map.Map b C.Type -> TypeVarEnv -> Bool -> MExcept (Map.Map b C.Type)
subTVars tEnv tVarMap allowPoly = DT.mapM (\t -> C.travTypesM t updateType) tEnv

  where
    -- try to substitute a tvar if it exists - this will behave differently depending on closed/open modules
    updateType :: C.Type -> MExcept C.Type
    updateType t@(C.TVar i) = case (Map.lookup i tVarMap) of
                                Just t' -> return t'
                                Nothing -> if allowPoly then return t else throwError "(TC03) - Type-variable found in non-polymorphic closed module"
    updateType t = return t



-- TODO - clean up
-- | run a map over the model replacing all bindings with typemap equiv
--typeExprs :: C.ExprMap Int -> TypeEnv -> C.ExprMap C.TypedId
--typeExprs exprMap tM = DF.foldl createModel OrdMap.empty newSeq
--  where
--    newSeq = fmap convBinding (OrdMap.elems exprMap)
--    convBinding t = fmap (\i -> C.TypedId i (tM Map.! i)) t
--    createModel m topExpr = OrdMap.insert b v m
--      where
--        (b, v) = C.getTopBinding topExpr

addConstraint :: C.Type -> C.Type -> TypeConsM ()
addConstraint t1 t2 = do
    tS <- lift get
    let tS' = Set.insert (t1, t2) tS
    lift $ put tS'

newTypevar :: TypeConsM C.Type
newTypevar = liftM C.TVar supply

-- | Adds a set of constraints for linking a multibind to a TVar
multiBindConstraint :: C.Bind Int -> C.Type -> TypeEnv -> TypeConsM TypeEnv
multiBindConstraint (C.LetBind bs) (C.TVar v) tEnv = do
    -- create the new tvars for each binding
    bTs <- mapM (\_ -> newTypevar) bs
    -- add the constaint
    addConstraint (C.TTuple bTs) (C.TVar v)
    -- add the tvars to the type map
    DF.foldlM (\tEnv (b, bT) -> return $ Map.insert b bT tEnv) tEnv (zip bs bTs)

constrain :: C.ExprMap Int -> ((TypeEnv, ModTypeEnv), TypeCons)
constrain exprMap = runState (evalSupplyT consM [1..]) (Set.empty)
  where
    consM :: TypeConsM (TypeEnv, ModTypeEnv)
    consM = DF.foldlM consTop (Map.empty, Map.empty) (OrdMap.elems exprMap)

    consTop (tEnv, mTEnv) (C.TopLet (C.LetBind bs) e) = do
        (eT, tEnv', mTEnv') <- consExpr tEnv mTEnv e
        -- extend and return tEnv
        case eT of
            (C.TTuple ts) -> return $ (foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv' (zip bs ts), mTEnv')
            (C.TVar v) | (length bs > 1) -> (multiBindConstraint (C.LetBind bs) (C.TVar v) tEnv') >>= (\tEnv -> return (tEnv, mTEnv'))
            t | length bs == 1 -> return $ (Map.insert (head bs) eT tEnv', mTEnv')
            _ -> trace (errorDump [show bs, show eT, show tEnv']) (error "(TYPECHECKER) - toplet shit\n")

    consTop (tEnv, mTEnv) (C.TopAbs (C.AbsBind b) arg e) = do
        fromT <- newTypevar
        -- extend the tEnv
        let tEnv' = Map.insert arg fromT tEnv
        (toT, tEnv'', mTEnv') <- consExpr tEnv' mTEnv e
        -- add a constraint?
        -- extend and return tEnv
        return $ (Map.insert b (C.TArr fromT toT) tEnv'', mTEnv')


    -- TODO - do we need to uniquely refer to each expression within AST?, or just bindings?
    -- | map over the expression elements, creating constraints as needed,
    consExpr :: TypeEnv -> ModTypeEnv -> C.Expr C.Id -> TypeConsM (C.Type, TypeEnv, ModTypeEnv)
    consExpr tEnv mTEnv (C.Var (C.LocalVar v)) = return $ (tEnv Map.! v, tEnv, mTEnv)

    -- TODO - tidy up
    -- need to look in a separate map here that adds the value on first lookup
    consExpr tEnv mTEnv (C.Var mv@(C.ModVar m v)) = do
        -- look into the mTEnv, if exists get the type, else create a newtvar and add to the map
        eT <- if (Map.member mv mTEnv) then return (mTEnv Map.! mv) else newTypevar
        let mTEnv' = Map.insert mv eT mTEnv
        return $ (eT, tEnv, mTEnv')

    consExpr tEnv mTEnv (C.Lit l) = return $ (getLitType l, tEnv, mTEnv)

    consExpr tEnv mTEnv (C.App (C.LocalVar f) e) = do
        -- as HOFs not allowed
        -- fT =  consExpr tEnv f
        let fT = tEnv Map.! f
        (eT, tEnv', mTEnv') <- consExpr tEnv mTEnv e
        toT <- newTypevar
        -- add constraint
        addConstraint fT (C.TArr eT toT)
        return (toT, tEnv', mTEnv')

    -- TODO - is this right?!
    consExpr tEnv mTEnv (C.App mv@(C.ModVar m v) e) = do
        -- similet to var - check if the type is already created, if not create a newTypeVar that
        -- we can constrain later
        fT <- if (Map.member mv mTEnv) then return (mTEnv Map.! mv) else newTypevar
        -- TODO - want to create and add in same step
        let mTEnv' = Map.insert mv fT mTEnv

        -- get the type of the expression
        (eT, tEnv', mTEnv'') <- consExpr tEnv mTEnv' e
        toT <- newTypevar
        -- add constraint
        addConstraint fT (C.TArr eT toT)
        return (toT, tEnv', mTEnv'')


    -- NOTE - do we need to return the new tEnv here?
    consExpr tEnv mTEnv (C.Let (C.LetBind bs) e1 e2) = do
        (e1T, tEnv', mTEnv') <- consExpr tEnv mTEnv e1
        -- extend tEnv with new env
        tEnv'' <- case e1T of
            (C.TTuple ts) -> return $ foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv' (zip bs ts)
            (C.TVar v) | (length bs > 1) -> multiBindConstraint (C.LetBind bs) (C.TVar v) tEnv'
            t | length bs == 1 -> return $ Map.insert (head bs) e1T tEnv'
            _ -> trace (errorDump [show bs, show e1T, show tEnv']) (error "(TYPECHECKER) - let shit\n")
        consExpr tEnv'' mTEnv' e2

    consExpr tEnv mTEnv (C.Op op e) = do
        let (C.TArr fromT toT) = getOpType op
        (eT, tEnv', mTEnv') <- consExpr tEnv mTEnv e
        -- NOTE - we don't need to gen a new tvar here as the totype is always fixed so the toT will always unify to it
        addConstraint fromT eT
        return (toT, tEnv', mTEnv')

    consExpr tEnv mTEnv (C.If eB eT eF) = do
        (eBT, tEnv', mTEnv') <- consExpr tEnv mTEnv eB
        addConstraint eBT C.TBool
        (eTT, tEnv'', mTEnv'') <- consExpr tEnv' mTEnv' eT
        (eFT, tEnv''', mTEnv''') <- consExpr tEnv'' mTEnv'' eF
        addConstraint eTT eFT
        return (eFT, tEnv''', mTEnv''')

    consExpr tEnv mTEnv (C.Tuple es) = liftM consTuple (DF.foldlM consElem ([], tEnv, mTEnv) es)
      where
        consElem (eTs, tEnv, mTEnv) e = consExpr tEnv mTEnv e >>= (\(eT, tEnv', mTEnv') -> return (eT:eTs, tEnv', mTEnv'))
        consTuple (eTs, tEnv, mTEnv) = (C.TTuple (reverse eTs), tEnv, mTEnv)

    -- other exprs
    consExpr tEnv mTEnv e = error ("(TC02) unknown expr - " ++ show e)


-- NOTE - should these two functions be moved into the AST?
getLitType :: C.Literal -> C.Type
getLitType l = case l of
    C.Boolean _ -> C.TBool
    C.Num _ -> C.TFloat
    C.NumSeq _ -> C.TFloat


-- | Takes an operator and returns the static type of the function
getOpType :: C.Op -> C.Type
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
    C.Not -> C.TArr C.TBool C.TBool
  where
    binNum = C.TArr (C.TTuple [C.TFloat, C.TFloat]) C.TFloat
    binRel = C.TArr (C.TTuple [C.TFloat, C.TFloat]) C.TBool
    binLog = C.TArr (C.TTuple [C.TBool, C.TBool]) C.TBool


-- | unify takes a set of type contraints and attempts to unify all types, inc TVars
-- based on HM - standard constraint unification algorithm
-- Bool argument determinst wheter the checking should allow polymophism and not fully-unify
unify :: TypeCons -> MExcept TypeVarEnv
unify tCons = liftM snd $ unify' (tCons, Map.empty)
  where
    unify' :: (TypeCons, TypeVarEnv) ->  MExcept (TypeCons, TypeVarEnv)
    unify' (tCons, tMap) = case (Set.minView tCons) of
                        Just (constraint, tCons') -> (uCon constraint (tCons', tMap)) >>= unify'
                        Nothing -> return (tCons, tMap)

    uCon :: (C.Type, C.Type) -> (TypeCons, TypeVarEnv) -> MExcept (TypeCons, TypeVarEnv)
    -- two equal ids - remove from set and ignore
    uCon (C.TVar xId, C.TVar yId) st
       | (xId == yId) = return st

    uCon (x, y) st
       | (x == y) = return st

    -- replace all x with y
    uCon (x@(C.TVar _), y) (tCons, tMap)
        | not (occursCheck x y) = return (subStack x y tCons, subMap x y tMap)

    -- replace all y with x
    uCon (x, y@(C.TVar _)) (tCons, tMap)
        | not (occursCheck y x) = return (subStack y x tCons, subMap y x tMap)

    -- composite types
    uCon (C.TArr x1 x2, C.TArr y1 y2) st = do
        st' <- uCon (x1, y1) st
        uCon (x2, y2) st'

    uCon (C.TTuple xs, C.TTuple ys) st = DF.foldlM (\st (x, y) -> uCon (x, y) st) st (zip xs ys)

    -- can't unfiy types
    uCon (x, y) st = trace (errorDump [show x, show y, show st]) $ throwError (printf "(TC01) - cannot unify %s and %s" (show x) (show y))

    -- replaces all occurances of tVar x with y in tCons
    subStack x y tCons = Set.map subTCon tCons
      where
        subTCon (a, b) = (subTTerm x y a, subTTerm x y b)

    -- replaces all occurances of tVar x with y in tMap
    subMap x@(C.TVar xId) y tMap = Map.insert xId y tMap'
      where
        tMap' = Map.map (subTTerm x y) tMap

    -- checks that tVar x does not exist in tTerm t, stop recursive substitions
    occursCheck x t@(C.TTuple ts)
        | t == x = True
        | otherwise = any (occursCheck x) ts
    occursCheck x t@(C.TArr fromT toT)
        | t == x = True
        | otherwise = (occursCheck x fromT) || (occursCheck x toT)
    occursCheck x t = if t == x then True else False

    -- replaces all occurances of x with y in the tTerm t
    subTTerm x y t@(C.TTuple ts)
        | t == x = y
        | otherwise = C.TTuple $ map (subTTerm x y) ts
    subTTerm x y t@(C.TArr fromT toT)
        | t == x = y
        | otherwise = C.TArr (subTTerm x y fromT) (subTTerm x y toT)
    subTTerm x y t = if t == x then y else t
