-----------------------------------------------------------------------------
--
-- Module      :  Process.Flatten.ConvertAST
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

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Process.Flatten.ConvertAST (
convertAST
) where

import Control.Monad.State
import Control.Conditional

import Utils.CommonImports
import Subsystem.SysState

import AST.Common
import AST.Module

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Utils.OrdMap as OrdMap
import qualified AST.Core as AC
import qualified AST.CoreFlat as ACF
import Utils.MonadSupply
import qualified Subsystem.Units as U
import qualified Subsystem.Types as T

-- Types ---------------------------------------------------------------------------------------------------------------
-- Same monad as renamer
type ConvM = SupplyT Id (StateT FlatState MExcept)
-- type ConvM = Supply Id

data FlatState = FlatState  { _curExprs :: ACF.ExprMap, _loopExprs :: ACF.ExprMap, _initExprs :: ACF.ExprMap
                            , _simOps :: [ACF.SimOps] , _inInit :: Bool, _curTMap :: TypeMap } deriving (Show, Eq, Ord)
mkFlatState = FlatState OrdMap.empty OrdMap.empty OrdMap.empty [] False

modInit :: Bool -> ConvM Bool
modInit isInit = do
    oldInit <- lift $ _inInit <$> get
    lift $ modify (\st -> st { _inInit = isInit || (_inInit st) } ) -- set Init flag
    return oldInit

setInit :: Bool -> ConvM ()
setInit isInit = lift $ modify (\st -> st { _inInit = isInit } ) -- set Init flag


-- Process Entry -------------------------------------------------------------------------------------------------------

-- TODO - need to create typedata in ACF.module too

convertAST :: Module Id -> MExcept ACF.Module
convertAST (LitMod modData) = do
    ((_, freeIds'), fSt') <- runStateT (runSupplyT flatExprM [freeId ..]) $ mkFlatState (modTMap modData)
    return $ ACF.Module (_loopExprs fSt') (_initExprs fSt') (reverse $ _simOps fSt') (head freeIds')
  where
    freeId = modFreeId modData
    flatExprM :: ConvM ()
    flatExprM = foldM_ convertTop () $ OrdMap.toList (modExprMap modData)

-- convert the toplet - we ensure that only TopLets with exist at this point
convertTop :: () -> ([Id], AC.TopLet Id) -> ConvM ()
convertTop _ (ids, AC.TopLet isInit t (ids') cE) = do
    assert (ids == ids') $ return ()
    setInit isInit
    convertLet t ids cE

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
    oldInit <- modInit isInit
    convertLet t bs e1
    setInit oldInit
    convertExpr e2

-- Literals
convertExpr e@(AC.Lit (AC.Num n U.NoUnit)) = return $ ACF.Var $ ACF.Num n
convertExpr e@(AC.Lit (AC.Boolean b)) = return $ ACF.Var $ ACF.Boolean b
convertExpr e@(AC.Lit (AC.Unit)) = return $ ACF.Var $ ACF.Unit
convertExpr e@(AC.Lit (AC.Time)) = return $ ACF.Var $ ACF.Time

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
        fT <- convertType <$> (lift . lift $ T.calcTypeExpr tMap e)
        let es = OrdMap.insert id (ACF.ExprData e' fT) ( _curExprs st')
        -- restore the old env
        lift . put $ st' { _curExprs = oldCurMap }
        return es

-- Tuple - delibeatly lift all refences here rather than try to embed, makes unpacking stage easier
convertExpr e@(AC.Tuple es) = ACF.Var <$> ACF.Tuple <$> mapM insertTmpVar es
-- Record - we convert to a tuple
convertExpr e@(AC.Record nEs) = ACF.Var <$> ACF.Tuple <$> mapM insertTmpVar (AC.dropLabels nEs)

-- Ode
convertExpr e@(AC.Ode (AC.LocalVar v) e1) = do
    -- convert the delta expr - insert in as an tmp binding
    vRef <- insertTmpVar e1
    -- add the Ode to SimOps
    lift $ modify (\st -> st { _simOps = (ACF.Ode v vRef) : (_simOps st) })
    -- add the vRef to the delta Expr to the cur exprMap
    return $ ACF.Var vRef


-- anything else,
convertExpr expr = errorDump [MkSB expr] "Cannot convert expression to CoreFlat" assert

-- Conversion Helper Functions -----------------------------------------------------------------------------------------

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
    maybe (insertTmpVar e) (return) mE'

-- convert an expr, create a new binding and return a refvar to it
insertTmpVar :: AC.Expr Id -> ConvM ACF.Var
insertTmpVar e = do
    -- convert the expression and return a var pointing to it
    id <- supply
    e' <- convertExpr e
    -- need to calc and convert the type here
    tMap <- _curTMap <$> lift get
    fT <- convertType <$> (lift . lift $ T.calcTypeExpr tMap e)
    insertExpr id e' fT
    return $ ACF.VarRef id

-- | Performs single look-ahead into the expression and lifts to a Var expr if possible
liftVarExpr :: AC.Expr Id -> ConvM (Maybe ACF.Var)
liftVarExpr e@(AC.Lit (AC.Num n U.NoUnit)) = return $ Just $ ACF.Num n
liftVarExpr e@(AC.Lit (AC.Boolean b)) = return $ Just $ ACF.Boolean b
liftVarExpr e@(AC.Lit (AC.Unit)) = return $ Just $ ACF.Unit
liftVarExpr e@(AC.Lit (AC.Time)) = return $ Just $ ACF.Time
liftVarExpr e@(AC.Var (AC.LocalVar v) Nothing) = return $ Just $ ACF.VarRef v
liftVarExpr e@(AC.Var (AC.LocalVar v) (Just recId)) = Just <$> convertRecId v recId
liftVarExpr e = return Nothing

-- | Wrapper function to insert a given expression into the correct exprmap under a given Id
insertExpr :: Id -> ACF.Expr -> ACF.Type -> ConvM ()
insertExpr id fE fT = do
    let exprData = ACF.ExprData fE fT
    ifM (_inInit <$> lift get)
        (lift $ modify (\st -> st { _initExprs = OrdMap.insert id exprData (_initExprs st) }))
        (lift $ modify (\st -> st { _loopExprs = OrdMap.insert id exprData (_loopExprs st) }))

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
