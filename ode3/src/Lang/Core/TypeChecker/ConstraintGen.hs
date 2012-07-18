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

import qualified UI.SysState as St
import Lang.Common.AST
import qualified Lang.Core.AST as E
import qualified Lang.Module.AST as M
import qualified Lang.Core.Units as U


import Lang.Core.TypeChecker.Common

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
                    E.Mul -> addConsSum $ ConsSum fTIn1 fTIn2 fTRet
                    E.Div -> addConsSum $ ConsSum fTIn2 fTRet fTIn1 -- a - b = c => a = b + c
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
        addConsSum $ ConsSum eDT (E.TFloat U.uSeconds) vT
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


