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

module Process.TypeChecker.ConstraintGen (
constrain, TypeEnvs(..)
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

import Utils.CommonImports
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap

import qualified Subsystem.SysState as St
import AST.Common
import AST.Ops as Ops
import qualified AST.Core as E
import qualified AST.Module as M
import qualified Subsystem.Units as U

import Process.TypeChecker.Common

-- Contraint Helpers Functions -----------------------------------------------------------------------------------------

-- constraint monad, generates type/unit vars within the constraint set
data TypeEnvs = TypeEnvs { typeEnv :: TypeEnv, recordTypeEnv :: RecordRefMap } deriving (Show, Eq, Ord)
mkTypeEnvs = TypeEnvs { typeEnv = Map.empty, recordTypeEnv = Map.empty }

type TypeConsM  = StateT TypeEnvs (SupplyT Int (StateT TypeCons MExcept))

liftMExcept = lift . lift . lift

addConsType :: ConType -> TypeConsM ()
addConsType con = lift . lift $ modify (\tCons -> tCons { conTypeS = Set.insert con (conTypeS tCons) })

addConsUnit :: ConUnit -> TypeConsM ()
addConsUnit con = lift . lift $ modify (\tCons -> tCons { conUnitS = Set.insert con (conUnitS tCons) })

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

getUnitFromEnv :: E.VarId E.Id -> TypeEnv -> Maybe U.Unit
getUnitFromEnv v tEnv = Map.lookup v tEnv >>= getUnit

getVarType :: E.VarId E.Id -> M.GlobalModEnv ->  M.ModData -> Maybe (M.FunArgs) -> TypeConsM E.Type
getVarType lv@(E.LocalVar _) _ _ _ = getLVarType lv
getVarType mv@(E.ModVar _ _) gModEnv modData mFuncArgs = getMVarType mv gModEnv modData mFuncArgs


-- have to use lifts as MonadSupply is only instance of MonadTrans, not MonadError
getLVarType :: E.VarId E.Id -> TypeConsM E.Type
getLVarType v@(E.LocalVar lv) = do
    tEnv <- typeEnv <$> get
    liftMExcept $ maybeToExcept (Map.lookup v tEnv) $ printf "Id %s not found in typeEnv" (show v)
getLVarType v = errorDump [MkSB v] "LocalVar expected" assert

-- | Obtains the type of a modVar reference, either from a fucntor or imported module if not
-- In the case of a functor, first it checks that the mod arg name is valid param,
-- if so then check if the type is already created, if not create a newTypeVar that we can constrain later
-- For in-module import, cheks the moduile has been imported, and then returns the final/fixed type of the reference
-- TODO - tidy up
getMVarType :: E.VarId E.Id -> M.GlobalModEnv ->  M.ModData -> Maybe (M.FunArgs) -> TypeConsM E.Type
getMVarType mv@(E.ModVar m v) gModEnv modData mFuncArgs =
    case mFuncArgs of
        Nothing         -> eLocalModEnv -- is ref to a localEnv module (thru imports/appMod)
        Just funcArgs   -> maybe eLocalModEnv id (eFunctor funcArgs) -- check funcArgs first, failing that then localModEnv
      where
        eLocalModEnv :: TypeConsM E.Type
        eLocalModEnv = do
            -- look in all module-level repos
            (_, impMod) <- liftMExcept $ M.getModuleMod m modData gModEnv
            -- if not found, raise error, else copy type into mTEnv
            eT <- liftMExcept $ M.lookupModSig v impMod
            modify (\tEnvs -> tEnvs { typeEnv = Map.insert mv eT (typeEnv tEnvs)})
            return eT

        -- tries to lookup the type in the functor args, if not
        eFunctor :: M.FunArgs -> Maybe (TypeConsM E.Type)
        eFunctor funcArgs = do
            -- does the mod name exist in functor's arg map
            case (OrdMap.lookup m funcArgs) of
                Nothing -> Nothing
                Just _ -> Just $ do
                    -- if so, then if v already exists get the type, else create a newtvar and add to the mTEnv
                    tEnvs@(TypeEnvs tEnv _) <- get
                    eT <- if (Map.member mv tEnv) then return (tEnv Map.! mv) else newTypevar
                    put $ tEnvs { typeEnv = Map.insert mv eT tEnv }
                    return eT


-- Binding Helper Functions --------------------------------------------------------------------------------------------

processLetBind :: E.BindList Int -> E.Type -> TypeConsM ()
processLetBind bs eT = do
    -- extend tEnv with new env
    tEnvs@(TypeEnvs tEnv _) <- get
    tEnv' <- case eT of
        -- true if tuple on both side of same size, if so unplack and treat as indivudual elems
        (E.TTuple ts) | (length bs == length ts) -> return $ foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv $ zip (map E.LocalVar bs) ts
        (E.TRecord ts) | (length bs == Map.size ts) -> return $ foldl (\tEnv (b, t) -> Map.insert b t tEnv) tEnv $ zip (map E.LocalVar bs) (Map.elems ts)
        -- true for individual elems, handle same as tuple above
        t | length bs == 1 -> return $ Map.insert (E.LocalVar . head $ bs) eT tEnv
        -- basic handling, is common case that subsumes special cases above, basically treat both sides as tuples
        -- (with tvars), create contstrains and let unificiation solve it instead
        t | (length bs > 1) -> tupleUnpackCons bs t tEnv
        _ -> errorDump [MkSB bs, MkSB eT, MkSB tEnv] "(TC) - let binding shit\n" assert
    put $ tEnvs { typeEnv = tEnv' }

-- | Adds a set of constraints for linking a multibind (from a tuple/record unpack) to a TVar
tupleUnpackCons :: E.BindList Int -> E.Type -> TypeEnv -> TypeConsM TypeEnv
tupleUnpackCons bs t tEnv = do
    -- create the new tvars for each binding
    bTs <- mapM (\_ -> newTypevar) bs
    -- add the constaint
    -- diff behavour and allow record-unpacking
    -- addConsType $ ConEqual (E.TRecord $ E.addLabels bTs) t
    addConsType $ ConEqual (E.TTuple $ bTs) t
    -- add the each of the tvars to the type map
    DF.foldlM (\tEnv (b, bT) -> return $ Map.insert b bT tEnv) tEnv $ zip (map E.LocalVar bs) bTs

-- | Update the tEnvs for records using the information collected within the RecordRefMap
recordRefsCons :: M.GlobalModEnv ->  M.ModData -> Maybe (M.FunArgs) -> TypeConsM ()
recordRefsCons gModEnv modData mFuncArgs = do
    -- get the data
    recEnv <- recordTypeEnv <$> get
    mapM_ addRecRefCons $ Map.toAscList recEnv
  where
    addRecRefCons (v, tInf@(E.TRecord nTs)) = do
        tCur <- getVarType v gModEnv modData mFuncArgs
        trace' [MkSB tInf, MkSB tCur] "Adding rec refs cons" $ return ()
        -- TODO - could make tInf a subtype here?
        addConsType $ ConEqual tInf tCur


-- Constraint Generation -----------------------------------------------------------------------------------------------

constrain :: M.GlobalModEnv ->  M.ModData -> Maybe (M.FunArgs) -> M.ExprMap Int -> MExcept (TypeEnvs, TypeCons)
constrain gModEnv modData mFuncArgs exprMap = runStateT (evalSupplyT (execStateT consM mkTypeEnvs) [1..]) mkTypeCons
  where
    consM :: TypeConsM ()
    consM = DF.mapM_ consTop (OrdMap.elems exprMap) >> recordRefsCons gModEnv modData mFuncArgs

    consTop (E.TopLet s bs e) = do
        eT <- consExpr e
        processLetBind bs eT

    -- is this right?
    consTop (E.TopType tName) = do
        -- create a wrapped typevar
        let lv = (E.LocalVar tName)
        -- TODO - this is wrong - should be a newtype cons, not an actual type
        tw <- E.TTypeCons (M.modFullName modData) <$> newTypevar
        -- extend and return tEnv
        modify (\tEnvs -> tEnvs { typeEnv = Map.insert lv tw (typeEnv tEnvs) })

    -- TODO - do we need to uniquely refer to each expression within AST?, or just bindings?
    -- | map over the expression elements, creating constraints as needed,
    consExpr :: E.Expr E.Id -> TypeConsM E.Type

    -- TODO - can auto-unit-convert the var access (local or at module boundary) here
    -- need to obtain the type of the module ref, either from functor or imported mod, creting a newTVar if needed
    consExpr (E.Var v Nothing) = getVarType v gModEnv modData mFuncArgs

    -- Handle record references within a varId
    consExpr e@(E.Var v (Just recId)) = do
        recEnv <- recordTypeEnv <$> get
        -- get the cur ref type
        case Map.lookup v recEnv of
            Just tRec@(E.TRecord ts) -> case Map.lookup recId ts of
                Just t -> return t
                Nothing -> addRecRef ts      -- update the inferered record
            Nothing -> addRecRef Map.empty   -- first reference, create an inferred record
            Just t -> errorDump [MkSB e, MkSB t] "Record reference TType pattern mismatch" assert
      where
        addRecRef ts = do
            tV <- newTypevar
            -- update the infered record
            let t' = E.TRecord $ Map.insert recId tV ts
            -- update the recEnv
            modify (\tEnvs -> tEnvs { recordTypeEnv = Map.insert v t' (recordTypeEnv tEnvs) })
            return tV

    -- TODO - can auto-unit-convert the function boundary here
    consExpr (E.App f e) = do
        -- as HOFs not allowed
        fT <- getVarType f gModEnv modData mFuncArgs
        -- app type logic
        -- get the type of the expression
        eT <- consExpr e
        toT <- newTypevar
        -- add constraint -- we constrain fT as (fT1->fT2) to eT->newTypeVar,
        -- rather than unpacking fT, constraining (fT1,eT) and returning fT2 - is simpler as newTypeVar will resolve to fT2 anyway
        addConsType $ ConEqual fT (E.TArr eT toT)
        return toT

    consExpr (E.Abs arg e) = do
        fromT <- newTypevar
        -- extend the tEnv
        modify (\tEnvs -> tEnvs { typeEnv = Map.insert (E.LocalVar arg) fromT (typeEnv tEnvs) })
        toT <- consExpr e
        -- add a constraint?
        return $ E.TArr fromT toT

    -- NOTE - do we need to return the new tEnv here?
    consExpr (E.Let s bs e1 e2) = do
        e1T <- consExpr e1
        processLetBind bs e1T
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
        addConsType $ ConEqual fromT eT
        -- addUnitCons fromU eU  -- how do we get eU ??
        -- plus can't as Units aren't composite, same prob as UC, need a parallel, redudant ADT
        -- altohugh we now toT is a float, we don't know the unit, so gen a UnitCons
        -- NOTE - we don't need to gen a new tvar for toT, as its type is always fixed so will always unify
        return toT

    consExpr (E.If eB eT eF) = do
        eBT <- consExpr eB
        addConsType $ ConEqual eBT E.TBool
        eTT <- consExpr eT
        eFT <- consExpr eF
        addConsType $ ConEqual eTT eFT
        return eFT

    consExpr (E.Tuple es) = liftM E.TTuple $ DT.mapM consExpr es

    -- is a literal record, record this within the type
    consExpr (E.Record nEs) = liftM E.TRecord $ DT.mapM consExpr nEs

    consExpr (E.Ode lv@(E.LocalVar _) eD) = do
        -- constrain the ode state val to be a float
        vT <- getLVarType lv
        uV1 <- newUnitVar
        addConsType $ ConEqual vT (E.TFloat uV1)

        -- add the deltaExpr type - must be Unit /s
        eDT <- consExpr eD
        uV2 <- newUnitVar
        addConsType $ ConEqual eDT (E.TFloat uV2)

        -- TODO - contrain both types wrt Time -- is this right?
        addConsUnit $ ConSum uV2 U.uSeconds uV1

        -- TODO - return the type of the dExpr
        return eDT

    consExpr (E.Rre src@(E.LocalVar _) dest@(E.LocalVar _) _) = do
        -- constrain both state vals to be floats
        srcT <- getLVarType src
        addConsType =<< ConEqual srcT <$> uFloat
        destT <- getLVarType dest
        addConsType =<< ConEqual destT <$> uFloat
        return E.TUnit

    -- Type/Unit-casting constraints
    consExpr (E.TypeCast e (E.UnitCast u)) = do
        -- get type of e
        eT <- consExpr e
        -- create unit for e
        uV1 <- newUnitVar
        addConsType $ ConEqual eT (E.TFloat uV1)
        -- constrain them both to be of the same dimension
        addConsUnit $ ConSameDim uV1 u
        -- return the new "casted" type
        return $ E.TFloat u

    -- TODO - need to look up within type env
    consExpr (E.TypeCast e (E.WrapType tName)) = do
        -- get type of e and wrap it with the typeName
        eT <- consExpr e
        -- get the stored newType type
        tTCons <- getVarType tName gModEnv modData mFuncArgs
        case tTCons of
            (E.TTypeCons modName tT) -> do
                -- add constraint
                addConsType $ ConEqual eT tT
                -- return wrapped type
                return $ E.TWrap modName (M.getVarSrcName tName modData)
            _ -> liftMExcept . throwError $ printf "(TC) Var %s is not a type constructor" (show tName)

    consExpr (E.TypeCast e (E.UnwrapType tName)) = do
        -- get type of e, should be wrapped with the typeName
        eTw <-  consExpr e
        -- get the stored newType type
        -- we could create a newTypeVar here instead of the pattern-match, but match should always succeed at this point
        tTCons <- getVarType tName gModEnv modData mFuncArgs
        case tTCons of
            (E.TTypeCons modName tT) -> do
                -- add constraint on wrapped types
                -- addConsType $ ConEqual fTw eTw
                addConsType $ ConEqual (E.TWrap modName (M.getVarSrcName tName modData)) eTw
                -- return unwrapped type
                return tT
            _ -> liftMExcept . throwError $ printf "(TC) Var %s is not a type constructor" (show tName)



    -- other exprs - not needed as match all
    consExpr e = errorDump [MkSB e] "(TC02) Unknown expr" assert



-- TODOs- check units for math ops, esp Mod, Pow

-- "static" function types for built-in ops,
-- creates the constraints interanal to the inputs and outputs of the op
-- (not callee/caller constraits)
getOpType :: Op -> TypeConsM E.Type
getOpType op = case op of
    -- Basic Ops
    BasicOp x | x `elem` [Add, Sub]                 -> typeAddSub'    -- (f u1, f u1) -> f u1
    BasicOp x | x `elem` [Mul, Div, Mod]            -> typeMulDiv'     -- (f u1, f u2) -> f u3
    BasicOp x | x `elem` [Ops.LT, LE, Ops.GT, GE, Ops.EQ, NEQ]  -> typeFFtoB'     -- (f u1, f u1) -> b
    BasicOp x | x `elem` [And, Or]                  -> typeBBtoB     -- (b, b) -> b
    BasicOp Not                                     -> typeBtoB     -- b -> b
    -- Math Ops
    MathOp x | x `elem` [ Sin, Cos, Tan, ASin, ACos, ATan, Exp, Exp2, Exp10, Pow10
                        , Log, Log2, Log10, LogB, Sqrt, Cbrt, ExpM1, Log1P
                        , SinH, CosH, TanH, ASinH, ACosH, ATanH
                        , Erf, ErfC, LGamma, Gamma, TGamma] -> typeFtoF        -- f -> f
    MathOp SinCos                                   -> typeFtoFF     -- f -> (f,f)
    MathOp x | x `elem` [ATan2, Pow, Hypot]         -> typeFFtoF     -- (f,f) -> f
  where
    -- add actual types info here
    -- basic NoUnit varients
    typeFFtoF   = return $ E.TArr (E.TTuple [E.TFloat U.NoUnit, E.TFloat U.NoUnit]) (E.TFloat U.NoUnit)
    -- typeFFtoB   = return $ E.TArr (E.TTuple [E.TFloat U.NoUnit, E.TFloat U.NoUnit]) E.TBool
    typeBBtoB   = return $ E.TArr (E.TTuple [E.TBool, E.TBool]) E.TBool
    typeBtoB    = return $ E.TArr E.TBool E.TBool
    typeFtoF    = return $ E.TArr (E.TFloat U.NoUnit) (E.TFloat U.NoUnit)
    typeFtoFF   = return $ E.TArr (E.TFloat U.NoUnit) (E.TTuple [E.TFloat U.NoUnit, E.TFloat U.NoUnit])

    -- Units varients
    -- (f u1, f u1) -> f u1
    typeAddSub' = do
        -- we could even create a new typevar here rather than unitVar, but then how to contrain to Floats?
        -- could do tVar<->Float UnknownUnit ? and then use speical rule in contrain-gen to replace all UnknownUnits
        floatT <- E.TFloat <$> newUnitVar
        -- use the same floatT for all them as they must all be the same type
        return $ E.TArr (E.TTuple [floatT, floatT]) floatT

    -- (f u1, f u1) -> b, with units
    typeFFtoB' = do
        floatT <- E.TFloat <$> newUnitVar
        return $ E.TArr (E.TTuple [floatT, floatT]) E.TBool

    -- (f u1, f u2) -> f u3, any units, mul/div semantics/constraint
    typeMulDiv' = do
        -- need create 3 unique uVars
        uV1 <- newUnitVar
        uV2 <- newUnitVar
        uV3 <- newUnitVar
        -- the inputs are indepedent, output depdendent on inputs - thus need special constraint rule, ConsSum
        case op of
            (BasicOp Mul) -> addConsUnit $ ConSum uV1 uV2 uV3
            (BasicOp Div) -> addConsUnit $ ConSum uV3 uV2 uV1 -- a - b = c => a = b + c
        return $ E.TArr (E.TTuple [E.TFloat uV1, E.TFloat uV2]) (E.TFloat uV3)



