-----------------------------------------------------------------------------
--
-- Module      :  Process.Flatten.ConvertAST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Process.Flatten.ConvertAST (
convertAST
) where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Utils.OrdMap as OrdMap


import Utils.CommonImports
import Utils.MonadSupply
-- import Subsystem.SysState
import qualified Subsystem.Units as U
import qualified Subsystem.Types as T
import qualified AST.Core as AC
import qualified AST.CoreFlat as ACF
import AST.Common
import AST.Module


-- Types ---------------------------------------------------------------------------------------------------------------
-- Same monad as renamer
type ConvM = SupplyT Id (StateT FlatState MExcept)
-- type ConvM = Supply Id

data FlatState = FlatState  { _curExprs :: ACF.ExprMap
                            , _simOps :: [ACF.SimOp]
                            , _curTMap :: TypeMap
                            , _timeUnit :: U.Unit
                            } deriving (Show, Eq, Ord)
mkFlatState = FlatState OrdMap.empty []

type InitMap = Map.Map Id Double
-- Entry Point ---------------------------------------------------------------------------------------------------------
-- TODO - need to create typedata in ACF.module too
-- we run conversion twice to handle split between init and loop exprs
-- this may result in redudnat/unused exprs in both blocks, however LLVM will remove these anyway, and can run interpreter over initExprs if needed
convertAST :: U.Unit -> (Module Id, InitMap) -> MExcept ACF.Module
convertAST tUnit (LitMod modData, initMap) = do
    -- trace' [MkSB modData] "Flatten - Final Core AST input" $ return ()
    ((_, freeIds'), fSt') <- runStateT (runSupplyT flatExprM freeIds) $ mkFlatState (modTMap modData) tUnit

    let simOps = reverse $ _simOps fSt'
    simType <- getSimType simOps

    trace' [MkSB simType] "Sim Type" $ return ()
    return $ ACF.Module (_curExprs fSt') initMap simOps simType (head freeIds')
  where
    freeIds = [modFreeId modData..]
    flatExprM :: ConvM ()
    flatExprM = foldM_ convertTop () $ OrdMap.toList (modExprMap modData)

    -- determine if this module may be simulated, has any simOps, and the mechanism to use
    getSimType simOps   | null simOps = throwError "(SM05) Final model does not contain any simulation operations (i.e. ODEs/SDEs/SSAs)"
                        | hasRREs && hasSDEs = throwError "(SM06) Hybrid simulation does not support mixed SSA and SDE simulation"
                        | hasRREs && hasODEs = return ACF.SimHybrid
                        | allRREs = return ACF.SimRRE
                        | hasSDEs = return ACF.SimSDE
                        | hasODEs = return ACF.SimODE
      where
        hasODEs = any checkODE simOps
        hasSDEs = any checkSDE simOps
        hasRREs = any checkRRE simOps
        allRREs = all checkRRE simOps
        checkODE op = case op of ACF.Ode _ _ -> True; _ -> False
        checkSDE op = case op of ACF.Sde _ _ _ -> True; _ -> False
        checkRRE op = case op of ACF.Rre _ _ _ -> True; _ -> False


-- convert the toplet - we ensure that only TopLets with exist at this point
convertTop :: () -> ([Id], AC.TopLet Id) -> ConvM ()
convertTop _ (ids, AC.TopLet isInit t ids' e) = do
    assert (ids == ids') $ return ()
    unless isInit $ convertLet t ids e

convertTop _ coreExpr = errorDump [MkSB coreExpr] "Cannot convert top expression to CoreFlat" assert


-- convert the expression, this is straightforwad for the resticted Core AST we have now anyway
-- puts it into ANF too
convertExpr :: AC.Expr Id -> ConvM ACF.Expr
-- Var
convertExpr e@(AC.Var (AC.LocalVar v) Nothing) = return $ ACF.Var (ACF.VarRef v)
-- Var with record ref - convert to a tuple ref
convertExpr e@(AC.Var (AC.LocalVar v) (Just recId)) = ACF.Var <$> convertRecId v recId

-- directly store the nested let bindings within the flattened exprMap
convertExpr e@(AC.Let isInit t bs e1 e2) = do
    unless isInit $ convertLet t bs e1
    convertExpr e2

-- Literals
convertExpr e@(AC.Lit (AC.Num n U.NoUnit)) = return $ ACF.Var $ ACF.Num n
convertExpr e@(AC.Lit (AC.Boolean b)) = return $ ACF.Var $ ACF.Boolean b
convertExpr e@(AC.Lit (AC.Unit)) = return $ ACF.Var ACF.Unit
convertExpr e@(AC.Lit (AC.Time)) = return $ ACF.Var ACF.Time
convertExpr e@(AC.Lit (AC.Wiener)) = return $ ACF.Var ACF.Wiener

-- Operators
-- multi-input op
convertExpr e@(AC.Op op (AC.Tuple es)) = do
    vs <- mapM convertVar es
    return $ ACF.Op op vs
-- single-input Op
convertExpr e@(AC.Op op e1) = do
    v <- convertVar e1
    return $ ACF.Op op [v]

-- If
-- TODO - is the ordering correct?
convertExpr e@(AC.If eB eT eF) = do
    vB <- convertVar eB
    esT <- createSubExprs eT
    esF <- createSubExprs eF
    return $ ACF.If vB esT esF
  where
    -- convert the expression within its own env
    createSubExprs e = do
        -- fucking record updates inside a state monad - so verbose!
        -- save the old env
        st <- lift $ get
        let oldCurMap = _curExprs st
        lift . put $ st { _curExprs = OrdMap.empty }
        -- actuall convert the expression - returns the ret val
        e' <- convertExpr e
        -- create a dummy value to handle the returned value (as our Lets are top-level, rather than let e1 in e2)
        id <- supply
        st' <- lift $ get
        -- need to calc and convert the type here
        tMap <- _curTMap <$> lift get
        tUnit <- _timeUnit <$> lift get
        fT <- convertType <$> (lift . lift $ T.calcTypeExpr (tMap, tUnit) e)
        let es = OrdMap.insert id (ACF.ExprData e' fT) ( _curExprs st')
        -- restore the old env
        lift . put $ st' { _curExprs = oldCurMap }
        return es

-- Tuple - delibeatly lift all refences here rather than try to embed, makes unpacking stage easier
convertExpr e@(AC.Tuple es) = ACF.Var <$> ACF.Tuple <$> mapM insertAsTmpVar es
-- Record - we convert to a tuple
convertExpr e@(AC.Record nEs) = ACF.Var <$> ACF.Tuple <$> mapM insertAsTmpVar (AC.dropLabels nEs)

-- Ode
convertExpr e@(AC.Ode (AC.LocalVar v) eD) = do
    -- convert the delta expr - insert in as an tmp binding
    dRef <- insertAsTmpVar eD
    -- add the Ode to SimOps
    lift $ modify (\st -> st { _simOps = (ACF.Ode v dRef) : (_simOps st) })
    -- add the vRef to the delta Expr to the cur exprMap
    return $ ACF.Var dRef

-- Sde
convertExpr e@(AC.Sde (AC.LocalVar v) eW eD) = do
    -- convert the wiener expr - insert in as an tmp binding
    wRef <- insertAsTmpVar eW
    -- convert the delta expr - insert in as an tmp binding
    dRef <- insertAsTmpVar eD
    -- add the Sde to SimOps
    lift $ modify (\st -> st { _simOps = (ACF.Sde v wRef dRef) : (_simOps st) })
    -- add the vRef to the delta Expr to the cur exprMap
    return $ ACF.Var dRef

-- Rre
convertExpr e@(AC.Rre srcs dests eR) = do
    -- convert the rate expr - insert in as an tmp binding
    rRef <- insertAsTmpVar eR
    -- add the Rre to SimOps
    lift $ modify (\st -> st { _simOps = (ACF.Rre (convProduct srcs) (convProduct dests) rRef) : (_simOps st) })
    -- return a unit val
    return $ ACF.Var ACF.Unit
  where
    convProduct = map (\(i, AC.LocalVar v) -> (fromInteger i, v))

-- Group function -- NYI in backend, we just replace with a unit val for now
convertExpr e@(AC.Group vs) = trace' [MkSB e] "Group function - NYI" $ return $ ACF.Var ACF.Unit

-- anything else,
convertExpr expr = errorDump [MkSB expr] "Cannot convert expression to CoreFlat" assert

-- Conversion Helper Functions -----------------------------------------------------------------------------------------

--convertInitVal :: AC.Type -> Id -> AC.Expr Id -> ConvM ()
--convertInitVal t id e1 = do
--    FlatState{_initMode} <- lift get
--
--    if _initMode
--        then do
--            -- convert the expr and type
--            fE <- convertExpr e1
--            let fT = convertType t
--            -- insert expr and add to initVals set
--            insertExpr id fE fT
--            lift $ modify (\st -> st { _initVals = Set.insert id (_initVals st) })
--        -- ignore the expression, it will already exist within initExprs
--        else return ()
--

-- convert a let-binding (both top and nested) into a global-let, handles unpacking a tuple references,
convertLet :: AC.Type -> AC.BindList Id -> AC.Expr Id -> ConvM ()
convertLet t ids e1 = do
    -- convert the expr and type
    fE <- convertExpr e1
    let fT = convertType t
    -- is it a multi-bind/tuple?
    case ids of
        (id:[]) -> insertExpr id fE fT
        _       -> do
            let (ACF.TTuple ts) = fT
            -- create a new var to hold the tuple
            tupleId <- supply
            insertExpr tupleId fE fT
            -- create a set of tupleaccess for expr (can't unpack directly as may be a Var, not direct Tuple)
            mapM_ (insertTupleRef tupleId) (zip3 ids ts [1..])
  where
    insertTupleRef :: Id -> (Id, ACF.Type, Integer) -> ConvM ()
    insertTupleRef tupleId (id, t, refIdx) = do
        insertExpr id (ACF.Var $ ACF.TupleRef tupleId refIdx) t

-- TODO - is this right?
-- should either lift/embed a var
convertVar :: AC.Expr Id -> ConvM ACF.Var
convertVar e = do
    mE' <- liftVarExpr e
    maybe (insertAsTmpVar e) (return) mE'

-- convert an expr, create a new binding and return a refvar to it
insertAsTmpVar :: AC.Expr Id -> ConvM ACF.Var
insertAsTmpVar e = do
    -- convert the expression and return a var pointing to it
    id <- supply
    e' <- convertExpr e
    -- need to calc and convert the type here
    tMap <- _curTMap <$> lift get
    tUnit <- _timeUnit <$> lift get
    fT <- convertType <$> (lift . lift $ T.calcTypeExpr (tMap, tUnit) e)
    insertExpr id e' fT
    return $ ACF.VarRef id

-- | Performs single look-ahead into the expression and lifts to a Var expr if possible
liftVarExpr :: AC.Expr Id -> ConvM (Maybe ACF.Var)
liftVarExpr e@(AC.Lit (AC.Num n U.NoUnit)) = return $ Just $ ACF.Num n
liftVarExpr e@(AC.Lit (AC.Boolean b)) = return $ Just $ ACF.Boolean b
liftVarExpr e@(AC.Lit (AC.Unit)) = return $ Just ACF.Unit
liftVarExpr e@(AC.Lit (AC.Time)) = return $ Just ACF.Time
liftVarExpr e@(AC.Lit (AC.Wiener)) = return $ Just ACF.Wiener
liftVarExpr e@(AC.Var (AC.LocalVar v) Nothing) = return $ Just $ ACF.VarRef v
liftVarExpr e@(AC.Var (AC.LocalVar v) (Just recId)) = Just <$> convertRecId v recId
liftVarExpr e = return Nothing


-- TODO - this is incorrect - need to insert into _curExprs
-- | Wrapper function to insert a given expression into the correct exprmap under a given Id
insertExpr :: Id -> ACF.Expr -> ACF.Type -> ConvM ()
insertExpr id fE fT = do
    let exprData = ACF.ExprData fE fT
    lift $ modify (\st -> st { _curExprs = OrdMap.insert id exprData (_curExprs st) })

-- | Convert a record Id reference to a tuple numerical reference
convertRecId :: Id -> String -> ConvM ACF.Var
convertRecId v recId = do
    -- we use the tMap to figure out the record label positioning as is flat- bit hacky but will work
    recT@(AC.TRecord nTs) <- lookupType v
    let tupIdx = toInteger . (+ 1) . fromJust . List.elemIndex recId . Map.keys $ nTs
    return $ ACF.TupleRef v tupIdx


-- Type Conversion -----------------------------------------------------------------------------------------------------
-- | lookups a type within the typemap
lookupType :: Id -> ConvM AC.Type
lookupType id = (Map.!) <$> (_curTMap <$> lift get) <*> pure id

-- | converts a Core type to a CoreFlat type
convertType :: AC.Type -> ACF.Type
convertType (AC.TBool)      = ACF.TBool
convertType (AC.TFloat _)   = ACF.TFloat
convertType (AC.TUnit)      = ACF.TUnit
convertType (AC.TTuple ts)  = ACF.TTuple $ map convertType ts
convertType (AC.TRecord nTs)  = ACF.TTuple $ map convertType (AC.dropLabels nTs)
convertType t = errorDump [MkSB t] "Unknown type during CoreFlat conv" assert

