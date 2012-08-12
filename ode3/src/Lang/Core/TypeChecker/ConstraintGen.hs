-----------------------------------------------------------------------------
--
-- Module      :  Lang.Core.TypeChecker.ConstraintGen
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

module Lang.Core.TypeChecker.ConstraintGen (
constrain
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

import qualified SysState as St
import Lang.Common.AST
import qualified Lang.Core.AST as E
import qualified Lang.Module.AST as M
import qualified Lang.Core.Units as U

import Lang.Core.TypeChecker.Common

-- Contraint Helpers Functions -----------------------------------------------------------------------------------------

-- constraint monad, generates type/unit vars within the constraint set
type TypeConsM  = StateT (TypeEnv, ModTypeEnv) (SupplyT Int (StateT TypeCons MExcept))

liftMExcept = lift . lift . lift

addConsEqual :: ConEqual -> TypeConsM ()
addConsEqual con = lift . lift $ modify (\tCons -> tCons { conEqualS = Set.insert con (conEqualS tCons) })

addConsSameDim :: ConSameDim -> TypeConsM ()
addConsSameDim con = lift . lift $ modify (\tCons -> tCons { conSameDimS = Set.insert con (conSameDimS tCons) })

addConsSum :: ConSum -> TypeConsM ()
addConsSum con = lift . lift $ modify (\tCons -> tCons { conSumS = Set.insert con (conSumS tCons) })

-- we handle both unitvars and typevars within the same supply monad, they only need to be unique, not sequential
newTypevar :: TypeConsM E.Type
newTypevar = E.TVar <$> lift (supply)

newUnitVar :: TypeConsM U.Unit
newUnitVar = U.UnitVar <$> lift (supply)

-- A new, float type wqith an "unknown" unit, should this be a UVar or UnknownUnit ?
uFloat = E.TFloat <$> newUnitVar

-- simple wrapper to extract the unit from a type, if possible
getUnit :: E.Type -> Maybe U.Unit
getUnit (E.TFloat u) = Just u
getUnit _ = Nothing

-- TypeEnv Functions ------------------------------------------------------------------------------

getUnitFromEnv :: E.Id -> TypeEnv -> Maybe U.Unit
getUnitFromEnv v tEnv = Map.lookup v tEnv >>= getUnit

-- have to use lifts as MonadSupply is only instance of MonadTrans, not MonadError
getType :: E.Id -> TypeConsM E.Type
getType v = do
    (tEnv, _) <- get
    liftMExcept $ maybeToExcept (Map.lookup v tEnv) $ printf "Id %s not found in typeEnv" (show v)

-- | Obtains the type of a modVar reference, either from a fucntor or imported module if not
-- In the case of a functor, first it checks that the mod arg name is valid param,
-- if so then check if the type is already created, if not create a newTypeVar that we can constrain later
-- For in-module import, cheks the moduile has been imported, and then returns the final/fixed type of the reference
-- TODO - tidy up
getMVarType :: E.VarId E.Id -> M.GlobalModEnv ->  M.ModData -> Maybe (M.FunArgs) -> TypeConsM E.Type
getMVarType mv@(E.ModVar m v) gModEnv modData mFuncArgs =
    case mFuncArgs of
        Nothing         -> eImport -- is in-module import only
        Just funcArgs   -> maybe eImport id (eFunctor funcArgs) -- check funcArgs first, failing that then in-module import
      where
        eImport :: TypeConsM E.Type
        eImport = do
            -- look in all module-level repos
            (_, impMod) <- liftMExcept $ M.getRealModuleMod m modData gModEnv
            -- if not found, raise error, el\se copy type into mTEnv
            eT <- liftMExcept $ M.getIdType v impMod
            modify (\(tEnv, mTEnv) -> (tEnv, Map.insert mv eT mTEnv))
            return eT

        -- tries to lookup the type in the functor args, if not
        eFunctor :: M.FunArgs -> Maybe (TypeConsM E.Type)
        eFunctor funcArgs = do
            -- does the mod name exist in functor's arg list
            case (OrdMap.lookup m funcArgs) of
                Nothing -> Nothing
                Just _ -> Just $ do
                    -- if so, then if v already exists get the type, else create a newtvar and add to the mTEnv
                    (tEnv, mTEnv) <- get
                    eT <- if (Map.member mv mTEnv) then return (mTEnv Map.! mv) else newTypevar
                    put (tEnv, Map.insert mv eT mTEnv)
                    return eT


-- Binding Helper Functions --------------------------------------------------------------------------------------------

-- | Adds a set of constraints for linking a multibind (from a tuple/record unpack) to a TVar
multiBindConstraint :: E.BindList Int -> E.Type -> TypeEnv -> TypeConsM TypeEnv
multiBindConstraint bs t tEnv = do
    -- create the new tvars for each binding
    bTs <- mapM (\_ -> newTypevar) bs
    -- add the constaint
    -- TODO - cosntraints to a record, ensuring only internally-convered tuples may be unpacked inline
    -- uncomment 2nd line below to switch behaviour and allow record-unpacking
    addConsEqual $ ConEqual (E.TRecord $ E.addLabels bTs) t
    -- addConsEqual $ ConEqual (E.TTuple $ bTs) t
    -- add the tvars to the type map
    DF.foldlM (\tEnv (b, bT) -> return $ Map.insert b bT tEnv) tEnv (zip bs bTs)


-- Constraint Generation -----------------------------------------------------------------------------------------------

constrain :: M.GlobalModEnv ->  M.ModData -> Maybe (M.FunArgs) -> M.ExprMap Int -> MExcept ((TypeEnv, ModTypeEnv), TypeCons)
constrain gModEnv modData mFuncArgs exprMap = runStateT (evalSupplyT (execStateT consM (Map.empty, Map.empty)) [1..]) mkTypeCons
  where
    consM :: TypeConsM ()
    consM = DF.mapM_ consTop (OrdMap.elems exprMap)

    consTop (E.TopLet s bs e) = do
        eT <- consExpr e
        (tEnv, mTEnv) <- get
        -- extend and return tEnv
        tEnv' <- case eT of
            -- true if tuple on both side of same size, if so unplack and treat as indivudual elems
            (E.TTuple ts) | (length bs == length ts) -> return $ foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv (zip bs ts)
            (E.TRecord ts) | (length bs == Map.size ts) -> return $ foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv (zip bs (Map.elems ts))
            -- true for individual elems, handle same as tuple above
            t | length bs == 1 -> return $ Map.insert (head bs) eT tEnv
            -- basic handling, is common case that subsumes special cases above, basically treat both sides as tuples
            -- (with tvars), create contstrains and let unificiation solve it instead
            t | (length bs > 1) -> multiBindConstraint bs t tEnv
            _ -> errorDump [MkSB bs, MkSB eT, MkSB tEnv] "(TC) - toplet shit"
        put (tEnv', mTEnv)

    -- is this right?
    consTop (E.TopType tName) = do
        (tEnv, mTEnv) <- get
        -- create a wrapped typevar
        tw <- E.TNewtype (E.LocalVar tName) <$> newTypevar
        -- extend and return tEnv
        let tEnv' = Map.insert tName tw tEnv
        put (tEnv', mTEnv)

--    consTop (tEnv, mTEnv) (E.TopLet (E.SingBind b) e) = do
--        (eT, tEnv', mTEnv') <- consExpr tEnv mTEnv e
--        -- true for individual elems, handle same as tuple above
--        return $ (Map.insert b eT tEnv', mTEnv')

    -- TODO - do we need to uniquely refer to each expression within AST?, or just bindings?
    -- | map over the expression elements, creating constraints as needed,

    consExpr :: E.Expr E.Id -> TypeConsM E.Type

    -- TODO - can auto-unit-convert at the var access here - but don't
    consExpr (E.Var (E.LocalVar v)) = getType v

    -- need to obtain the type of the module ref, either from functor or imported mod, creting a newTVar if needed
    -- TODO - can auto-unit-convert at the module boundary here
    consExpr (E.Var mVar@(E.ModVar m v)) = getMVarType mVar gModEnv modData mFuncArgs

    -- TODO - can auto-unit-convert at the function boundary here
    consExpr (E.App (E.LocalVar f) e) = do
        -- as HOFs not allowed
        -- fT =  consExpr tEnv f
        fT <- getType f
        eT <- consExpr e
        toT <- newTypevar
        -- add constraint
        addConsEqual $ ConEqual fT (E.TArr eT toT)
        return toT

    -- TODO - is this right?!
    -- TODO - can auto-unit-convert at the module boundary here
    consExpr (E.App mVar@(E.ModVar m v) e) = do
        -- similar to localVar
        fT <- getMVarType mVar gModEnv modData mFuncArgs
        -- app type logic
        -- get the type of the expression
        eT <- consExpr e
        toT <- newTypevar
        -- add constraint -- we constrain fT as (fT1->fT2) to eT->newTypeVar,
        -- rather than unpacking fT, constraining (fT1,eT) and returning fT2 - is simpler as newTypeVar will resolve to fT2 anyway
        addConsEqual $ ConEqual fT (E.TArr eT toT)
        return toT

    consExpr (E.Abs arg e) = do
        fromT <- newTypevar
        -- extend the tEnv
        modify (\(tEnv, mTEnv) -> (Map.insert arg fromT tEnv, mTEnv))
        toT <- consExpr e
        -- add a constraint?
        return $ E.TArr fromT toT

    -- NOTE - do we need to return the new tEnv here?
    consExpr (E.Let s bs e1 e2) = do
        e1T <- consExpr e1
        -- extend tEnv with new env
        (tEnv, mTEnv) <- get
        tEnv' <- case e1T of
            -- true if tuple on both side of same size, if so unplack and treat as indivudual elems
            (E.TTuple ts) | (length bs == length ts) -> return $ foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv (zip bs ts)
            (E.TRecord ts) | (length bs == Map.size ts) -> return $ foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv (zip bs (Map.elems ts))
            -- true for individual elems, handle same as tuple above
            t | length bs == 1 -> return $ Map.insert (head bs) e1T tEnv
            -- basic handling, is common case that subsumes special cases above, basically treat both sides as tuples
            -- (with tvars), create contstrains and let unificiation solve it instead
            t | (length bs > 1) -> multiBindConstraint bs t tEnv
            _ -> errorDump [MkSB bs, MkSB e1T, MkSB tEnv] "(TC) - let shit\n"
        -- now constrain e2 using the new typeEnv
        put (tEnv', mTEnv)
        consExpr e2

    -- Mapping from literal -> type
    consExpr (E.Lit l) = case l of
        E.Boolean _ -> return E.TBool
        -- should this be of unit NoUnit or UnitVar ??
        -- E.Num _ -> uFloat
        E.Num _ u -> return $ E.TFloat u
        E.NumSeq _ u -> return $ E.TFloat  u
        E.Time -> return $ E.TFloat U.uSeconds -- should this be uFloat ??
        E.Unit -> return E.TUnit

    -- test add, same code for most ops (not mul/div)
    consExpr (E.Op op e) = do
        --get "static" function type
        (E.TArr fromT toT) <- getOpType op
        -- get the arg type
        eT <- consExpr e
        -- add callee/caller constraints
        addConsEqual $ ConEqual fromT eT
        -- addUnitCons fromU eU  -- how do we get eU ??
        -- plus can't as Units aren't composite, same prob as UC, need a parallel, redudant ADT
        -- altohugh we now toT is a float, we don't know the unit, so gen a UnitCons
        -- NOTE - we don't need to gen a new tvar for toT, as its type is always fixed so will always unify
        return toT
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
                floatT <- E.TFloat <$> newUnitVar
                -- use the same floatT for all them as they must all be the same type
                return $ E.TArr (E.TTuple [floatT , floatT]) floatT

            -- we need to constrict these to all the same unit
            -- binAddSub = E.TArr (E.TTuple [(E.TFloat U.UnknownUnit), (E.TFloat U.UnknownUnit)]) (E.TFloat U.UnknownUnit)

            -- binary, (F,F) -> B, all equal type
            binRel = do
                floatT <- E.TFloat <$> newUnitVar
                return $ E.TArr (E.TTuple [floatT , floatT]) E.TBool

            -- binary, (F,F) -> F, any units, mul/div semantics/constraint
            binMulDiv = do
                -- need create 3 unique uVars
                uV1 <- newUnitVar
                uV2 <- newUnitVar
                uV3 <- newUnitVar
                -- the inputs are indepedent, output depdendent on inputs - thus need special constraint rule, ConsMul
                case op of
                    E.Mul -> addConsSum $ ConSum uV1 uV2 uV3
                    E.Div -> addConsSum $ ConSum uV3 uV2 uV1 -- a - b = c => a = b + c
                return $ E.TArr (E.TTuple [E.TFloat uV1, E.TFloat uV2]) (E.TFloat uV3)

    consExpr (E.If eB eT eF) = do
        eBT <- consExpr eB
        addConsEqual $ ConEqual eBT E.TBool
        eTT <- consExpr eT
        eFT <- consExpr eF
        addConsEqual $ ConEqual eTT eFT
        return eFT

    consExpr (E.Tuple es) = liftM E.TTuple $ DT.mapM consExpr es

    consExpr (E.Record nEs) = liftM E.TRecord $ DT.mapM consExpr nEs

    consExpr (E.Ode (E.LocalVar v) eD) = do
        -- constrain the ode state val to be a float
        vT <- getType v
        uV1 <- newUnitVar
        addConsEqual $ ConEqual vT (E.TFloat uV1)

        -- add the deltaExpr type - must be Unit /s
        eDT <- consExpr eD
        uV2 <- newUnitVar
        addConsEqual $ ConEqual eDT (E.TFloat uV2)

        -- TODO - contrain both types wrt Time -- is this right?
        addConsSum $ ConSum uV2 U.uSeconds uV1
        return E.TUnit

    consExpr (E.Rre (E.LocalVar src) (E.LocalVar dest) _) = do
        -- constrain both state vals to be floats
        srcT <- getType src
        addConsEqual =<< ConEqual srcT <$> uFloat
        destT <- getType dest
        addConsEqual =<< ConEqual destT <$> uFloat
        return E.TUnit

    -- explicit cast->number here -- to handle case that convcast and inital unit creation are overloaded
--    consExpr (E.ConvCast (E.Lit (E.Num n u1)) u2) = do
--        -- no contraints needed, direct cast
--        return $ E.TFloat u

    consExpr (E.TypeCast e (E.UnitCast u)) = do
        -- get type of e
        eT <- consExpr e
        -- create unit for e
        uV1 <- newUnitVar
        addConsEqual $ ConEqual eT (E.TFloat uV1)
        -- constrain them both to be of the same dimension
        addConsSameDim $ ConSameDim uV1 u
        -- return the new "casted" type
        return $ E.TFloat u


    -- TODO - need to look up within type env
    consExpr (E.TypeCast e (E.WrapType t@(E.LocalVar v))) = do
        -- get type of e and wrap it
        eTw <- E.TNewtype t <$> consExpr e
        -- get the stored newType type
        fTw <- getType v
        trace' [MkSB eTw, MkSB fTw] "Wrap Types" $ return ()
        -- add constraint
        addConsEqual $ ConEqual fTw eTw
        -- return wrapped type
        return eTw

    consExpr (E.TypeCast e (E.UnwrapType t@(E.LocalVar v))) = do
        -- get type of e, should be wrapped
        eTw <-  consExpr e
        -- get the stored newType type
        -- we could create a newTypeVar here instead of the pattern-match, but match should always succeed at this point

        -- trace' [MkSB eTw, MkSB fTw] "Unwrap Types" $ return ()
        fTw@(E.TNewtype _ fT)<- getType v

        trace' [MkSB eTw, MkSB fTw] "Unwrap Types" $ return ()
        -- add constraint on wrapped types
        addConsEqual $ ConEqual fTw eTw
        -- return unwrapped type
        return fT

--        -- get type of e
--        eT <- consExpr e
--        -- ensure eT is a wrapped type
--        tV <- newTypevar
--        addConsEqual $ ConEqual eT (E.TNewtype t tV)
--        -- anything else??
--        return $ tV

    -- other exprs - not needed as match all
    consExpr e = errorDump [MkSB e] "(TC02) Unknown expr"
