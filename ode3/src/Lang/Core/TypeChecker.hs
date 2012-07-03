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
-- Auxillary Checks Performed
-- * Referenced (module) binding does exist (within import scope)
-- * if import - is module in importMap, and if so does var exist
-- * if functor - is mod name equal to a functor arg name

-----------------------------------------------------------------------------
{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

module Lang.Core.TypeChecker (
typeCheck -- , typeCheckApp, TypeVarEnv, TypeCons, unify
) where

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Text.Printf (printf)
import Lang.Common.AST
import qualified Lang.Core.AST as E
import qualified Lang.Module.AST as M
import Utils.Utils
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap
import qualified Lang.Core.Units as U


-- Types ---------------------------------------------------------------------------------------------------------------

type TypeEnv    = M.TypeMap
type TypeVarEnv = Map.Map E.Id E.Type

-- type env for modules, only one needed as vars should be unique,
-- * for imports copies the type from the global modEnv
-- * for functor holds a type var for the first occurance of a module var
type ModTypeEnv = Map.Map (E.VarId E.Id) E.Type

type TypeCons   = Set.Set (E.Type, E.Type)
type TypeConsM  = SupplyT Int (StateT TypeCons MExcept)

-- Main Interface ------------------------------------------------------------------------------------------------------

-- TODO - un-Do this!

typeCheck :: M.GlobalModEnv -> M.FileData -> M.Module E.Id -> MExcept (M.Module E.Id)
typeCheck gModEnv fileData mod@(M.LitMod exprMap modData) = do
    -- get the contraints
    ((tEnv, mTEnv), tCons) <- constrain gModEnv modData Nothing exprMap

    -- unify the types and get the new typemap
    tVarMap <- unify tCons
    -- substitute to obtain the new type env
    tEnv' <- subTVars tEnv tVarMap False
    let modData' = updateModData modData tEnv'

    -- trace ("(TC) " ++ show exprMap) ()
    return $ M.LitMod exprMap modData'

typeCheck gModEnv fileData mod@(M.FunctorMod args exprMap modData) = do
    -- get the contraints
    ((tEnv, mTEnv), tCons) <- constrain gModEnv modData (Just args) exprMap

    -- unify the types and get the new typemap
    tVarMap <- unify tCons
    -- substitute to obtain the new type env
    tEnv' <- subTVars tEnv tVarMap True
    let modData' = updateModData modData tEnv'

    -- functor specific type-checking
    mTEnv' <- subTVars mTEnv tVarMap True
    let args' = createFunModArgs args mTEnv'

    return $ M.FunctorMod args' exprMap modData'
  where
    -- create the public module signatures for Functors
    createFunModArgs :: M.FunArgs -> ModTypeEnv -> M.FunArgs
    createFunModArgs args mTEnv = Map.foldrWithKey addArg args mTEnv
      where
        -- add the type for M.v into the args OrdMap
        addArg (E.ModVar m v) t args = OrdMap.update updateModArgs m args
          where
            updateModArgs modMap = Just (Map.insert v t modMap)


-- takes the funcModule, an closed enviroment of the module args,
--typeCheckApp :: M.Module E.Id -> M.FileModEnv ->  MExcept (M.Module E.Id, M.FileModEnv)
--typeCheckApp fMod@(M.FunctorMod funArgs _ _) modEnv = do
--    -- get the complete typevar map for an application, create a new set of constraints based on the requried and given mod sigs
--    tCons <- DF.foldlM constrainSigs Set.empty (OrdMap.toList funArgs)
--    -- unify these constraints and create a new type sub map
--    tVarMap <- unify tCons
--    -- update the module with new type submap
--    fMod' <- updateMod tVarMap fMod
--    modEnv' <- DT.mapM (updateMod tVarMap) modEnv
--    return (fMod', modEnv')
--  where
--    -- take a single mod arg for the functor, compare the expected sig with the actual one for the id within the modEnv,
--    -- add all sigs to the typeCons via a foldr
--    constrainSigs :: TypeCons -> (ModName, M.SigMap) -> MExcept TypeCons
--    constrainSigs typeCons (funcArgId, funcArgSig) = DF.foldrM compareTypes typeCons (Map.toList funcArgSig)
--      where
--        -- the module referenced by the arg
--        argMod@(M.LitMod _ argModData) = modEnv Map.! funcArgId
--        -- comparing the types for each binding by adding to the typeconstraint set
--        compareTypes (b,tFunc) typeCons = typeCons'
--          where
--            typeCons' = case (Map.lookup b (M.modSig argModData)) of
--                Just tArg -> Right $ Set.insert (tFunc, tArg) typeCons
--                Nothing -> throwError $ "(MO05) - Invalid functor signature, cannot find ref " ++ show b ++ " in module argument" ++ show funcArgId
--
--    -- update a module based on the new/more-complete type map - substitutiong tVars for concrete types
--    updateMod :: TypeEnv -> M.Module E.Id -> MExcept (M.Module E.Id)
--    updateMod tVarMap (M.LitMod exprMap modData) = do
--        tEnv' <- subTVars (M.modTMap modData) tVarMap False
--        let modData' = updateModData modData tEnv'
--        return $ M.LitMod exprMap modData'
--
--    updateMod tVarMap (M.FunctorMod args exprMap modData) = do
--        tEnv' <- subTVars (M.modTMap modData) tVarMap False
--        let modData' = updateModData modData tEnv'
--        -- create the new funArgs based on the new tVarMap
--        args' <- DT.mapM (\idMap -> subTVars idMap tVarMap False) args
--        return $ M.FunctorMod args' exprMap modData'


-- | Update the module data with the public module signature and internal typemap
-- we create the mod signature by mapping over the idbimap data and looking up each value from the internal typemap
updateModData :: M.ModData -> TypeEnv -> M.ModData
updateModData modData tEnv = modData { M.modTMap = tEnv, M.modSig = modSig }
  where
    idMap = Bimap.toMap (M.modIdBimap modData)
    modSig = Map.map (\id -> tEnv Map.! id) idMap


-- | use the TVar map to undate a type enviroment and substitute all TVars
-- Bool argument determinst wheter the checking should allow polymophism and not fully-unify
subTVars :: Show b => Map.Map b E.Type -> TypeVarEnv -> Bool -> MExcept (Map.Map b E.Type)
subTVars tEnv tVarMap allowPoly = DT.mapM (\t -> E.travTypesM t updateType) tEnv
  where
    -- try to substitute a tvar if it exists - this will behave differently depending on closed/open modules
    updateType :: E.Type -> MExcept E.Type
    updateType t@(E.TVar i) = case (Map.lookup i tVarMap) of
                                Just t' -> return t'
                                Nothing -> if allowPoly then return t else
                                            trace' [MkSB tEnv, MkSB tVarMap] "Poly error" $ throwError "(TC03) - Type-variable found in non-polymorphic closed module"
    updateType t = return t


-- Constraint Generation -----------------------------------------------------------------------------------------------

addConstraint :: E.Type -> E.Type -> TypeConsM ()
addConstraint t1 t2 = do
    tS <- lift $ get
    let tS' = Set.insert (t1, t2) tS
    lift $ put tS'

newTypevar :: TypeConsM E.Type
newTypevar = E.TVar <$> supply


-- | Adds a set of constraints for linking a multibind to a TVar
multiBindConstraint :: E.Bind Int -> E.Type -> TypeEnv -> TypeConsM TypeEnv
multiBindConstraint (E.Bind bs) t tEnv = do
    -- create the new tvars for each binding
    bTs <- mapM (\_ -> newTypevar) bs
    -- add the constaint
    addConstraint (E.TTuple bTs) t
    -- add the tvars to the type map
    DF.foldlM (\tEnv (b, bT) -> return $ Map.insert b bT tEnv) tEnv (zip bs bTs)

-- | Obtains the type of a modVar reference, either from a fucntor or imported module if not
-- In the case of a functor, first it checks that the mod arg name is valid param,
-- if so then check if the type is already created, if not create a newTypeVar that we can constrain later
-- For in-module import, cheks the moduile has been imported, and then returns the fixed type of the reference
-- TODO - tidy up
getMVarType :: M.GlobalModEnv ->  M.ModData -> Maybe (M.FunArgs) -> ModTypeEnv -> E.VarId E.Id -> TypeConsM (E.Type, ModTypeEnv)
getMVarType gModEnv modData mFuncArgs mTEnv mv@(E.ModVar m v) =
    case mFuncArgs of
        Nothing         -> eImport -- is in-module import only
        Just funcArgs   -> maybe eImport id (eFunctor funcArgs) -- check funcArgs first, failing that then in-module import
      where
        eImport :: TypeConsM (E.Type, ModTypeEnv)
        eImport = case eImport' of
            Left err    -> lift . lift $ throwError err
            Right res   -> return res

        eImport' :: MExcept (E.Type, ModTypeEnv)
        eImport' = do
            -- look in all module-level repos
            (_, impMod) <- M.getRealModuleMod m modData gModEnv
            -- if not found, raise error, el\se copy type into mTEnv
            eT <- M.getIdType v impMod
            return $ (eT, Map.insert mv eT mTEnv)

        -- tries to lookup the type in the functor args, if not
        eFunctor :: M.FunArgs -> Maybe (TypeConsM (E.Type, ModTypeEnv))
        eFunctor funcArgs = do
            -- does the mod name exist in functor's arg list
            case (OrdMap.lookup m funcArgs) of
                Nothing -> Nothing
                Just _ -> Just $ do
                    -- if so, then if v already exists get the type, else create a newtvar and add to the mTEnv
                    eT <- if (Map.member mv mTEnv) then return (mTEnv Map.! mv) else newTypevar
                    return $ (eT, Map.insert mv eT mTEnv)


constrain :: M.GlobalModEnv ->  M.ModData -> Maybe (M.FunArgs) -> M.ExprMap Int -> MExcept ((TypeEnv, ModTypeEnv), TypeCons)
constrain gModEnv modData mFuncArgs exprMap = runStateT (evalSupplyT consM [1..]) (Set.empty)
  where
    consM :: TypeConsM (TypeEnv, ModTypeEnv)
    consM = DF.foldlM consTop (Map.empty, Map.empty) (OrdMap.elems exprMap)

    consTop (tEnv, mTEnv) (E.TopLet s (E.Bind bs) e) = do
        (eT, tEnv', mTEnv') <- consExpr tEnv mTEnv e
        -- extend and return tEnv
        case eT of
            -- true if tuple on both side of same size, if so unplack and treat as indivudual elems
            (E.TTuple ts) | (length bs == length ts) -> return $ (foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv' (zip bs ts), mTEnv')
            -- true for individual elems, handle same as tuple above
            t | length bs == 1 -> return $ (Map.insert (head bs) eT tEnv', mTEnv')
            -- basic handling, is common case that subsumes special cases above, basically treat both sides as tuples
            -- (with tvars), create contstrains and let unificiation solve it instead
            t | (length bs > 1) -> (multiBindConstraint (E.Bind bs) t tEnv') >>= (\tEnv -> return (tEnv, mTEnv'))
            _ -> errorDump [MkSB bs, MkSB eT, MkSB tEnv'] "(TYPECHECKER) - toplet shit\n"

--    consTop (tEnv, mTEnv) (E.TopLet (E.SingBind b) e) = do
--        (eT, tEnv', mTEnv') <- consExpr tEnv mTEnv e
--        -- true for individual elems, handle same as tuple above
--        return $ (Map.insert b eT tEnv', mTEnv')

    -- TODO - do we need to uniquely refer to each expression within AST?, or just bindings?
    -- | map over the expression elements, creating constraints as needed,
    consExpr :: TypeEnv -> ModTypeEnv -> E.Expr E.Id -> TypeConsM (E.Type, TypeEnv, ModTypeEnv)
    consExpr tEnv mTEnv (E.Var (E.LocalVar v)) = return $ (tEnv Map.! v, tEnv, mTEnv)


    -- need to obtain the type of the module ref, either from functor or imported mod, creting a newTVar if needed
    consExpr tEnv mTEnv (E.Var mv@(E.ModVar m v)) = do
        (eT, mTEnv') <- getMVarType gModEnv modData mFuncArgs mTEnv mv
        -- general ret
        return $ (eT, tEnv, mTEnv')

    consExpr tEnv mTEnv (E.Lit l) = return $ (E.getLitType l, tEnv, mTEnv)

    consExpr tEnv mTEnv (E.App (E.LocalVar f) e) = do
        -- as HOFs not allowed
        -- fT =  consExpr tEnv f
        let fT = tEnv Map.! f
        (eT, tEnv', mTEnv') <- consExpr tEnv mTEnv e
        toT <- newTypevar
        -- add constraint
        addConstraint fT (E.TArr eT toT)
        return (toT, tEnv', mTEnv')

    -- TODO - is this right?!
    consExpr tEnv mTEnv (E.App mv@(E.ModVar m v) e) = do
        -- similar to Var
        (fT, mTEnv') <- getMVarType gModEnv modData mFuncArgs mTEnv mv
        -- app type logic
        -- get the type of the expression
        (eT, tEnv', mTEnv'') <- consExpr tEnv mTEnv' e
        toT <- newTypevar
        -- add constraint -- we constrain fT as (fT1->fT2) to eT->newTypeVar,
        -- rather than unpacking fT, constraining (fT1,eT) and returning fT2 - is simpler as newTypeVar will resolve to fT2 anyway
        addConstraint fT (E.TArr eT toT)
        return (toT, tEnv', mTEnv'')

    consExpr tEnv mTEnv (E.Abs arg e) = do
        fromT <- newTypevar
        -- extend the tEnv
        let tEnv' = Map.insert arg fromT tEnv
        (toT, tEnv'', mTEnv') <- consExpr tEnv' mTEnv e
        -- add a constraint?
        return $ (E.TArr fromT toT, tEnv'', mTEnv')


    -- NOTE - do we need to return the new tEnv here?
    consExpr tEnv mTEnv (E.Let s (E.Bind bs) e1 e2) = do
        (e1T, tEnv', mTEnv') <- consExpr tEnv mTEnv e1
        -- extend tEnv with new env
        tEnv'' <- case e1T of
            -- true if tuple on both side of same size, if so unplack and treat as indivudual elems
            (E.TTuple ts) | (length bs == length ts) -> return $ foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv' (zip bs ts)
            -- true for individual elems, handle same as tuple above
            t | length bs == 1 -> return $ Map.insert (head bs) e1T tEnv'
            -- basic handling, is common case that subsumes special cases above, basically treat both sides as tuples
            -- (with tvars), create contstrains and let unificiation solve it instead
            t | (length bs > 1) -> multiBindConstraint (E.Bind bs) t tEnv'
            _ -> errorDump [MkSB bs, MkSB e1T, MkSB tEnv'] "(TYPECHECKER) - let shit\n"
        consExpr tEnv'' mTEnv' e2


    consExpr tEnv mTEnv (E.Op op e) = do
        let (E.TArr fromT toT) = E.getOpType op
        (eT, tEnv', mTEnv') <- consExpr tEnv mTEnv e
        -- NOTE - we don't need to gen a new tvar here as the totype is always fixed so the toT will always unify to it
        addConstraint fromT eT
        return (toT, tEnv', mTEnv')

    consExpr tEnv mTEnv (E.If eB eT eF) = do
        (eBT, tEnv', mTEnv') <- consExpr tEnv mTEnv eB
        addConstraint eBT E.TBool
        (eTT, tEnv'', mTEnv'') <- consExpr tEnv' mTEnv' eT
        (eFT, tEnv''', mTEnv''') <- consExpr tEnv'' mTEnv'' eF
        addConstraint eTT eFT
        return (eFT, tEnv''', mTEnv''')

    consExpr tEnv mTEnv (E.Tuple es) = liftM consTuple (DF.foldlM consElem ([], tEnv, mTEnv) es)
      where
        consElem (eTs, tEnv, mTEnv) e = consExpr tEnv mTEnv e >>= (\(eT, tEnv', mTEnv') -> return (eT:eTs, tEnv', mTEnv'))
        consTuple (eTs, tEnv, mTEnv) = (E.TTuple (reverse eTs), tEnv, mTEnv)

    consExpr tEnv mTEnv (E.Ode (E.LocalVar v) eD) = do
        -- constrain the ode state val to be a float
        let vT = tEnv Map.! v
        addConstraint vT E.uFloat
        -- add the deltaExpr type
        (eDT, tEnv', mTEnv') <- consExpr tEnv mTEnv eD
        addConstraint eDT E.uFloat
        return (E.TUnit, tEnv', mTEnv')

    consExpr tEnv mTEnv (E.Rre (E.LocalVar src) (E.LocalVar dest) _) = do
        -- constrain both state vals to be floats
        let srcT = tEnv Map.! src
        addConstraint srcT E.uFloat
        let destT = tEnv Map.! dest
        addConstraint destT E.uFloat
        return (E.TUnit, tEnv, mTEnv)

    consExpr tEnv mTEnv (E.ConvCast e _) = do
        -- constrain e to be a float
        (eT, tEnv', mTEnv') <- consExpr tEnv mTEnv e
        addConstraint eT E.uFloat
        return $ (E.uFloat, tEnv', mTEnv')

    -- other exprs - not needed as match all
    consExpr tEnv mTEnv e = errorDump [MkSB e] "(TC02) Unknown expr"


-- Constraint Unification ----------------------------------------------------------------------------------------------

-- | unify takes a set of type contraints and attempts to unify all types, inc TVars
-- based on HM - standard constraint unification algorithm
unify :: TypeCons -> MExcept TypeVarEnv
unify tCons = --trace' [MkSB tCons] "Initial Unify tCons" $
    liftM snd $ unify' (tCons, Map.empty)
  where
    unify' :: (TypeCons, TypeVarEnv) ->  MExcept (TypeCons, TypeVarEnv)
    unify' (tCons, tMap) = --trace' [MkSB tCons, MkSB tMap] "Unify iteration" $ case (Set.minView tCons) of
                        case (Set.minView tCons) of
                            Just (constraint, tCons') -> (uCon constraint (tCons', tMap)) >>= unify'
                            Nothing -> return (tCons, tMap)

    uCon :: (E.Type, E.Type) -> (TypeCons, TypeVarEnv) -> MExcept (TypeCons, TypeVarEnv)
    -- two equal ids - remove from set and ignore
    uCon (E.TVar xId, E.TVar yId) st
       | (xId == yId) = return st

    -- replace all x with y
    uCon (x@(E.TVar xId), y) (tCons, tMap)
        | not (occursCheck x y) = case Map.lookup xId tMap of
                                    Just xT -> return (Set.insert (xT, y) tCons, tMap) -- the tvar has already been updated, use this and recheck
                                    --Just xT -> uCon (xT, y) (tCons, tMap) -- can also, but neater to reinsert into the tCons
                                    Nothing -> return (subStack x y tCons, subAddMap x y tMap) -- tvar doesn't exist, add and sub

    -- replace all y with x
    uCon (x, y@(E.TVar _)) st = uCon (y, x) st
--        | not (occursCheck y x) = return (subStack y x tCons, subMap y x tMap)

    -- composite types
    uCon (E.TArr x1 x2, E.TArr y1 y2) st = do
        st' <- uCon (x1, y1) st
        uCon (x2, y2) st'

    uCon (E.TTuple xs, E.TTuple ys) st | (length xs == length ys) = DF.foldlM (\st (x, y) -> uCon (x, y) st) st (zip xs ys)

    uCon (x, y) st
       | (x == y) = return st

    -- can't unfiy types
    uCon (x, y) st = trace' [MkSB x, MkSB y, MkSB st] "Type Error" $ throwError (printf "(TC01) - cannot unify %s and %s" (show x) (show y))

    -- replaces all occurances of tVar x with y in tCons
    subStack x y tCons = Set.map subTCon tCons
      where
        subTCon (a, b) = (subTTerm x y a, subTTerm x y b)

    -- replaces all occurances of (tVar x) with y in tMap, then add [x->y] to the tMap
    subAddMap x@(E.TVar xId) y tMap = Map.insert xId y tMap'
      where
        tMap' = Map.map (subTTerm x y) tMap

    -- checks that tVar x does not exist in tTerm t, stop recursive substitions
    occursCheck x t@(E.TTuple ts)
        | t == x = True
        | otherwise = any (occursCheck x) ts
    occursCheck x t@(E.TArr fromT toT)
        | t == x = True
        | otherwise = (occursCheck x fromT) || (occursCheck x toT)
    occursCheck x t = if t == x then True else False

    -- replaces all occurances of x with y in the tTerm t
    subTTerm x y t@(E.TTuple ts)
        | t == x = y
        | otherwise = E.TTuple $ map (subTTerm x y) ts
    subTTerm x y t@(E.TArr fromT toT)
        | t == x = y
        | otherwise = E.TArr (subTTerm x y fromT) (subTTerm x y toT)
    subTTerm x y t = if t == x then y else t
