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

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Error

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT

import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Text.Printf (printf)

import Utils.Utils
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap

import qualified UI.SysState as St
import Lang.Common.AST
import qualified Lang.Core.AST as E
import qualified Lang.Module.AST as M
import qualified Lang.Core.Units as U



-- Global Types ---------------------------------------------------------------------------------------------------------------

type TypeEnv = M.TypeMap
type TypeVarEnv = Map.Map E.Id E.Type

-- type env for modules, only one needed as vars should be unique,
-- * for imports copies the type from the global modEnv
-- * for functor holds a type var for the first occurance of a module var
type ModTypeEnv = Map.Map (E.VarId E.Id) E.Type

-- Main Interface ------------------------------------------------------------------------------------------------------

-- TODO - un-Do this!

typeCheck :: M.GlobalModEnv -> M.FileData -> St.UnitsState -> M.Module E.Id -> MExcept (M.Module E.Id)
typeCheck gModEnv fileData uState mod@(M.LitMod exprMap modData) = do
    -- get the contraints
    ((tEnv, mTEnv), tCons) <- constrain gModEnv modData Nothing exprMap

    -- unify the types and get the new typemap
    tVarMap <- unify tCons
    -- substitute to obtain the new type env
    tEnv' <- subTVars tEnv tVarMap False
    let modData' = updateModData modData tEnv'

    -- trace ("(TC) " ++ show exprMap) ()
    return $ M.LitMod exprMap modData'

typeCheck gModEnv fileData uState mod@(M.FunctorMod args exprMap modData) = do
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
-- doesn't exists - check VCS history

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

-- set of contraints for the module - include both the types and a set of rules that determine the
-- unit level contraints
--data UnitsConRule   = --NoUnit    -- type?/float does not have unit information
----                    | SameUnit  -- floats must be contrained to same unit
--                      Equal     -- Types must be fully equal, regardless if they have units
----                    | SameDim   -- floats must be contrained to same dimenstion, effectively restricted unit-polymorpihism
----                                -- we could add full unit-polymorphism by creating unit-vars for a particular dimenstions and constraining (again) later
--                    deriving (Show, Eq, Ord)

-- true for both types and units ?
data ConsEqual = ConsEqual E.Type E.Type deriving (Show, Eq, Ord)
-- should this be Unit, not type??
data ConsSameDim = ConsSameDim E.Type E.Type deriving (Show, Eq, Ord)
-- can unify Mul&Div into ConsSum (a,b) = c
data ConsSum = ConsSum (E.Type, E.Type) E.Type deriving (Show, Eq, Ord)

data TypeCons = TypeCons    { consEquals :: Set.Set ConsEqual
                            , consSums :: Set.Set ConsSum
                            , consSameDims :: Set.Set ConsSameDim
                            } deriving (Show, Eq, Ord)

mkTypeCons = TypeCons Set.empty Set.empty Set.empty

--data ConsRule   = ConsEqual E.Type E.Type       -- true for both types and units ?
--                | ConsSameDim E.Type E.Type     -- should this be Unit, not type??
--                -- ConsMul E.Type E.Type E.Type  -- contrain the results from a multiplcation
--                -- ConsDiv E.Type E.Type E.Type  -- contrain the results from a div
--                -- can unify Mul&Div into ConsSum (a,b) = c
--                | ConsSum (E.Type, E.Type) E.Type
--                -- unit stuff -- can we do at the float level?
--                -- ConsSameUnit Unit Unit     -- is needed?
--                deriving (Show, Eq, Ord)

-- type TypeCons   = Set.Set ConsRule
-- constraint monad, generates type/unit vars within the constraint set
type TypeConsM  = SupplyT Int (StateT TypeCons MExcept)


addConsEqual :: ConsEqual -> TypeConsM ()
addConsEqual cons = lift $ modify (\tCons -> tCons { consEquals = Set.insert cons (consEquals tCons) })

addConsSameDim :: ConsSameDim -> TypeConsM ()
addConsSameDim cons = lift $ modify (\tCons -> tCons { consSameDims = Set.insert cons (consSameDims tCons) })

addConsSum :: ConsSum -> TypeConsM ()
addConsSum cons = lift $ modify (\tCons -> tCons { consSums = Set.insert cons (consSums tCons) })



newTypevar :: TypeConsM E.Type
newTypevar = E.TVar <$> supply

-- we handle both unitvars and typevars within the same supply monad, they only need to be unique, not sequential
newUnitVar :: TypeConsM U.Unit
newUnitVar = U.UnitVar <$> supply

-- returns a new unitvar encapsulated within a Float type
newUnitVarFloat :: TypeConsM E.Type
newUnitVarFloat = E.TFloat <$> newUnitVar

-- A new, float type wqith an "unknown" unit, should this be a UVar or UnknownUnit ?
uFloat = newUnitVarFloat

getUnitForId :: TypeEnv -> E.Id -> Maybe U.Unit
getUnitForId tEnv v = Map.lookup v tEnv >>= getUnitForType

-- simple wrapper to extract the unit from a type, if possible
getUnitForType :: E.Type -> Maybe U.Unit
getUnitForType (E.TFloat u) = Just u
getUnitForType _ = Nothing

-- | Adds a set of constraints for linking a multibind to a TVar
multiBindConstraint :: E.Bind Int -> E.Type -> TypeEnv -> TypeConsM TypeEnv
multiBindConstraint (E.Bind bs) t tEnv = do
    -- create the new tvars for each binding
    bTs <- mapM (\_ -> newTypevar) bs
    -- add the constaint
    addConsEqual $ ConsEqual (E.TTuple bTs) t
    -- add the tvars to the type map
    DF.foldlM (\tEnv (b, bT) -> return $ Map.insert b bT tEnv) tEnv (zip bs bTs)

-- | Obtains the type of a modVar reference, either from a fucntor or imported module if not
-- In the case of a functor, first it checks that the mod arg name is valid param,
-- if so then check if the type is already created, if not create a newTypeVar that we can constrain later
-- For in-module import, cheks the moduile has been imported, and then returns the final/fixed type of the reference
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
constrain gModEnv modData mFuncArgs exprMap = runStateT (evalSupplyT consM [1..]) mkTypeCons
  where
    consM :: TypeConsM (TypeEnv, ModTypeEnv)
    consM = DF.foldlM consTop (Map.empty, Map.empty) (OrdMap.elems exprMap)

    consTop (tEnv, mTEnv) (E.TopLet s (E.Bind bs) e) = do
        (tEnv', mTEnv', eT) <- consExpr tEnv mTEnv e
        -- extend and return tEnv
        case eT of
            -- true if tuple on both side of same size, if so unplack and treat as indivudual elems
            (E.TTuple ts) | (length bs == length ts) -> return $ (foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv' (zip bs ts), mTEnv')
            -- true for individual elems, handle same as tuple above
            t | length bs == 1 -> return $ (Map.insert (head bs) eT tEnv', mTEnv')
            -- basic handling, is common case that subsumes special cases above, basically treat both sides as tuples
            -- (with tvars), create contstrains and let unificiation solve it instead
            t | (length bs > 1) -> (multiBindConstraint (E.Bind bs) t tEnv') >>= (\tEnv -> return (tEnv, mTEnv'))
            _ -> errorDump [MkSB bs, MkSB eT, MkSB tEnv'] "(TC) - toplet shit\n"

--    consTop (tEnv, mTEnv) (E.TopLet (E.SingBind b) e) = do
--        (eT, tEnv', mTEnv') <- consExpr tEnv mTEnv e
--        -- true for individual elems, handle same as tuple above
--        return $ (Map.insert b eT tEnv', mTEnv')

    -- TODO - do we need to uniquely refer to each expression within AST?, or just bindings?
    -- | map over the expression elements, creating constraints as needed,

    consExpr :: TypeEnv -> ModTypeEnv -> E.Expr E.Id -> TypeConsM (TypeEnv, ModTypeEnv, E.Type)

    -- TODO - can auto-unit-convert at the var access here - but don't
    consExpr tEnv mTEnv (E.Var (E.LocalVar v)) = return $ (tEnv, mTEnv, tEnv Map.! v)

    -- need to obtain the type of the module ref, either from functor or imported mod, creting a newTVar if needed
    -- TODO - can auto-unit-convert at the module boundary here
    consExpr tEnv mTEnv (E.Var mv@(E.ModVar m v)) = do
        (eT, mTEnv') <- getMVarType gModEnv modData mFuncArgs mTEnv mv
        -- general ret
        return $ (tEnv, mTEnv', eT)

    -- TODO - can auto-unit-convert at the function boundary here
    consExpr tEnv mTEnv (E.App (E.LocalVar f) e) = do
        -- as HOFs not allowed
        -- fT =  consExpr tEnv f
        let fT = tEnv Map.! f
        (tEnv', mTEnv', eT) <- consExpr tEnv mTEnv e
        toT <- newTypevar
        -- add constraint
        addConsEqual $ ConsEqual fT (E.TArr eT toT)
        return (tEnv', mTEnv', toT)

    -- TODO - is this right?!
    -- TODO - can auto-unit-convert at the module boundary here
    consExpr tEnv mTEnv (E.App mv@(E.ModVar m v) e) = do
        -- similar to Var
        (fT, mTEnv') <- getMVarType gModEnv modData mFuncArgs mTEnv mv
        -- app type logic
        -- get the type of the expression
        (tEnv', mTEnv'', eT) <- consExpr tEnv mTEnv' e
        toT <- newTypevar
        -- add constraint -- we constrain fT as (fT1->fT2) to eT->newTypeVar,
        -- rather than unpacking fT, constraining (fT1,eT) and returning fT2 - is simpler as newTypeVar will resolve to fT2 anyway
        addConsEqual $ ConsEqual fT (E.TArr eT toT)
        return (tEnv', mTEnv'', toT)

    consExpr tEnv mTEnv (E.Abs arg e) = do
        fromT <- newTypevar
        -- extend the tEnv
        let tEnv' = Map.insert arg fromT tEnv
        (tEnv'', mTEnv', toT) <- consExpr tEnv' mTEnv e
        -- add a constraint?
        return $ (tEnv'', mTEnv', E.TArr fromT toT)


    -- NOTE - do we need to return the new tEnv here?
    consExpr tEnv mTEnv (E.Let s (E.Bind bs) e1 e2) = do
        (tEnv', mTEnv', e1T) <- consExpr tEnv mTEnv e1
        -- extend tEnv with new env
        tEnv'' <- case e1T of
            -- true if tuple on both side of same size, if so unplack and treat as indivudual elems
            (E.TTuple ts) | (length bs == length ts) -> return $ foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv' (zip bs ts)
            -- true for individual elems, handle same as tuple above
            t | length bs == 1 -> return $ Map.insert (head bs) e1T tEnv'
            -- basic handling, is common case that subsumes special cases above, basically treat both sides as tuples
            -- (with tvars), create contstrains and let unificiation solve it instead
            t | (length bs > 1) -> multiBindConstraint (E.Bind bs) t tEnv'
            _ -> errorDump [MkSB bs, MkSB e1T, MkSB tEnv'] "(TC) - let shit\n"
        consExpr tEnv'' mTEnv' e2

    consExpr tEnv mTEnv (E.Lit l) = getLitType l >>= (\lT -> return $ (tEnv, mTEnv, lT))
      where
        -- | Mapping from literal -> type
        getLitType :: E.Literal -> TypeConsM E.Type
        getLitType l = case l of
            E.Boolean _ -> return E.TBool
            E.Num _ -> uFloat
            E.NumSeq _ -> uFloat
            E.Time -> return $ E.TFloat U.uSeconds -- should this be uFloat ??
            E.Unit -> return E.TUnit

    -- test add, same code for most ops (not mul/div)
    consExpr tEnv mTEnv (E.Op op e) = do
        --get "static" function type
        (E.TArr fromT toT) <- getOpType op
        -- get the arg type
        (tEnv', mTEnv', eT) <- consExpr tEnv mTEnv e
        -- add callee/caller constraints
        addConsEqual $ ConsEqual fromT eT
        -- addUnitCons fromU eU  -- how do we get eU ??
        -- plus can't as Units aren't composite, same prob as UC, need a parallel, redudant ADT
        -- altohugh we now toT is a float, we don't know the unit, so gen a UnitCons
        -- NOTE - we don't need to gen a new tvar for toT, as its type is always fixed so will always unify
        return (tEnv', mTEnv', toT)
      where
        -- "static" function types for built-in ops,
        -- creates the constraints interanal to the inputs and outputs of the op
        -- (not callee/caller constraits)
        getOpType :: E.Op -> TypeConsM E.Type
        getOpType op = case op of
            E.Add   -> binAddSub
            E.Sub   -> binAddSub
            E.Mul   -> binMulDiv
            E.Div   -> binMulDiv
            E.Mod   -> binAddSub -- TODO - is this right?
            E.LT    -> binRel
            E.LE    -> binRel
            E.GT    -> binRel
            E.GE    -> binRel
            E.EQ    -> binRel
            E.NEQ   -> binRel
            E.And   -> return $ E.TArr (E.TTuple [E.TBool, E.TBool]) E.TBool
            E.Or    -> return $ E.TArr (E.TTuple [E.TBool, E.TBool]) E.TBool
            E.Not   -> return $ E.TArr E.TBool E.TBool
          where
            -- binary, (F,F) -> F, all equal type
            binAddSub = do
                -- we could even create a new typevar here rather than unitVar, but then how to contrain to Floats?
                -- could do tVar<->Float UnknownUnit ? and then use speical rule in contrain-gen to replace all UnknownUnits
                floatT <- newUnitVarFloat
                -- use the same floatT for all them as they must all be the same type
                return $ E.TArr (E.TTuple [floatT , floatT]) floatT

            -- we need to constrict these to all the same unit
            -- binAddSub = E.TArr (E.TTuple [(E.TFloat U.UnknownUnit), (E.TFloat U.UnknownUnit)]) (E.TFloat U.UnknownUnit)

            -- binary, (F,F) -> B, all equal type
            binRel = do
                floatT <- newUnitVarFloat
                return $ E.TArr (E.TTuple [floatT , floatT]) E.TBool

            -- binary, (F,F) -> F, any units, mul/div semantics/constraint
            binMulDiv = do
                -- need create 3 unique uVars
                fTIn1 <- newUnitVarFloat
                fTIn2 <- newUnitVarFloat
                fTRet <- newUnitVarFloat
                -- the inputs are indepedent, output depdendent on inputs - thus need special constraint rule, ConsMul
                case op of
                    --E.Mul -> addConstraint $ ConsMul fTIn1 fTIn2 fTRet
                    --E.Div -> addConstraint $ ConsDiv fTIn1 fTIn2 fTRet
                    E.Mul -> addConsSum $ ConsSum (fTIn1, fTIn2) fTRet
                    E.Div -> addConsSum $ ConsSum (fTIn2, fTRet) fTIn1 -- a - b = c => a = b + c
                return $ E.TArr (E.TTuple [fTIn1, fTIn2]) fTRet

    consExpr tEnv mTEnv (E.If eB eT eF) = do
        (tEnv', mTEnv', eBT) <- consExpr tEnv mTEnv eB
        addConsSameDim $ ConsSameDim eBT E.TBool
        (tEnv'', mTEnv'', eTT) <- consExpr tEnv' mTEnv' eT
        (tEnv''', mTEnv''', eFT) <- consExpr tEnv'' mTEnv'' eF

        addConsEqual $ ConsEqual eTT eFT
        return (tEnv''', mTEnv''', eFT)

    consExpr tEnv mTEnv (E.Tuple es) = liftM consTuple (DF.foldlM consElem (tEnv, mTEnv, []) es)
      where
        consElem (tEnv, mTEnv, eTs) e = consExpr tEnv mTEnv e >>= (\(tEnv', mTEnv', eT) -> return (tEnv', mTEnv', eT:eTs))
        consTuple (tEnv, mTEnv, eTs) = (tEnv, mTEnv, E.TTuple (reverse eTs))

    consExpr tEnv mTEnv (E.Ode (E.LocalVar v) eD) = do
        -- constrain the ode state val to be a float
        let vT = tEnv Map.! v
        addConsEqual =<< ConsEqual vT <$> uFloat
        -- add the deltaExpr type - must be Unit /s
        (tEnv', mTEnv', eDT) <- consExpr tEnv mTEnv eD
        addConsEqual =<< ConsEqual eDT <$> uFloat
        -- TODO - contrain both types wrt Time -- is this right?
        addConsSum $ ConsSum (eDT, E.TFloat U.uSeconds) vT
        return (tEnv', mTEnv', E.TUnit)

    consExpr tEnv mTEnv (E.Rre (E.LocalVar src) (E.LocalVar dest) _) = do
        -- constrain both state vals to be floats
        let srcT = tEnv Map.! src
        addConsEqual =<< ConsEqual srcT <$> uFloat
        let destT = tEnv Map.! dest
        addConsEqual =<< ConsEqual destT <$> uFloat
        return (tEnv, mTEnv, E.TUnit)

    consExpr tEnv mTEnv (E.ConvCast e u) = do
        -- get type of e
        (tEnv', mTEnv', eT) <- consExpr tEnv mTEnv e
        -- create ret type
        let toT = E.TFloat u
        -- constrain them to both be of the same dimension
        addConsSameDim $ ConsSameDim eT toT
        -- return the new "casted" type
        return (tEnv', mTEnv', toT)

    -- other exprs - not needed as match all
    consExpr tEnv mTEnv e = errorDump [MkSB e] "(TC02) Unknown expr"


-- Constraint Unification ----------------------------------------------------------------------------------------------

type UnifyM = StateT TypeVarEnv MExcept

-- | unify takes a set of type contraints and attempts to unify all types, inc TVars
-- based on HM - standard constraint unification algorithm
unify :: TypeCons -> MExcept TypeVarEnv
unify tCons = snd <$> runStateT unifyM Map.empty
  where
    --trace' [MkSB tCons] "Initial Unify tCons" $
    unifyM = unifyEquals tCons >>= unifySum >>= unifySameDim


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
    -- two equal ids - remove from set and ignore
    unifyEquals' (ConsEqual (E.TVar xId) (E.TVar yId)) tCons
       | (xId == yId) = return tCons

    -- replace all x with y
    unifyEquals' (ConsEqual x@(E.TVar xId) y) tCons
        | not (occursCheck x y) = do
            tEnv <- get
            case Map.lookup xId tEnv of
                 -- the tvar has already been updated, use this and recheck, reinsert a new equality constraint
                Just xT -> return $ tCons { consEquals = Set.insert (ConsEqual xT y) (consEquals tCons)}
                --Just xT -> uCon (xT, y) (tCons, tEnv) -- can also, but neater to reinsert into the tCons
                Nothing -> do
                    _ <- modify (\tEnv -> subAddMap x y tEnv)
                    return $ subStack x y tCons -- tvar doesn't exist, add and sub

    -- replace all y with x
    unifyEquals' (ConsEqual x y@(E.TVar _)) st = unifyEquals' (ConsEqual y x) st
    --        | not (occursCheck y x) = return (subStack y x tCons, subMap y x tEnv)

    -- composite types -- do we allow these any more as need to apply Units rules at a base unit??
    -- level, thus composites do not allow thiw compatible
    unifyEquals' (ConsEqual (E.TArr x1 x2) (E.TArr y1 y2)) st = do
        st' <- unifyEquals' (ConsEqual x1 y1) st
        unifyEquals' (ConsEqual x2 y2) st'

    unifyEquals' (ConsEqual (E.TTuple xs) (E.TTuple ys)) st | (length xs == length ys) =
        DF.foldlM (\st (x, y) -> unifyEquals' (ConsEqual x y) st) st (zip xs ys)

    -- base unit, we do the main checking here for uRules
    unifyEquals' (ConsEqual x y) st | (x == y) = return st
    -- uCon (x, y, uRule) st = undefined -- are these needed?

    -- can't unify types
    unifyEquals' (ConsEqual x y) st = trace' [MkSB x, MkSB y, MkSB st] "Type Error" $ throwError (printf "(TC01) - cannot unify %s and %s" (show x) (show y))



-- TODO - other rules here
-- uCon ConsSameDim -- check UC convCast rule
-- uCon ConsSum -- check UC op rules

-- | (Some Unification) and Checking for Sum rule
unifySum = undefined

-- | (Not really unification) and Checking for SameDim rule
unifySameDim = undefined


-- | replaces all occurances of tVar x with y in tCons
subStack :: E.Type -> E.Type -> TypeCons -> TypeCons
subStack x y tCons = tCons { consEquals = consEquals', consSums = consSums', consSameDims = consSameDims' }
  where
    consEquals' = Set.map (\(ConsEqual a b) -> (ConsEqual (subTTerm x y a) (subTTerm x y b))) (consEquals tCons)
    consSums' = Set.map (\(ConsSum (a,b) c) -> (ConsSum ((subTTerm x y a), (subTTerm x y b)) (subTTerm x y c))) (consSums tCons)
    consSameDims' = Set.map (\(ConsSameDim a b) -> (ConsSameDim (subTTerm x y a) (subTTerm x y b))) (consSameDims tCons)

-- | replaces all occurances of (tVar x) with y in tEnv, then add [x->y] to the tEnv
subAddMap x@(E.TVar xId) y tEnv = Map.insert xId y tEnv'
  where
    tEnv' = Map.map (subTTerm x y) tEnv

-- | checks that tVar x does not exist in tTerm t, stop recursive substitions
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
