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

import Utils.CommonImports
import Subsystem.SysState

import AST.Common
import AST.Module

import qualified Data.Map as Map
import qualified Utils.OrdMap as OrdMap
import qualified AST.Core as AC
import qualified AST.CoreFlat as ACF
import Utils.MonadSupply
import qualified Subsystem.Units as U

import Process.Flatten.ConvertTypes(calcTypeExpr)

-- Types ---------------------------------------------------------------------------------------------------------------
-- Same monad as renamer
type ConvM = SupplyT Id (StateT FlatState MExcept)
-- type ConvM = Supply Id

data FlatState = FlatState  { curExprs :: ACF.ExprMap, loopExprs :: ACF.ExprMap,  initExprs :: ACF.ExprMap
                            , inInit :: Bool, curTMap :: TypeMap } deriving (Show, Eq, Ord)
mkFlatState = FlatState OrdMap.empty OrdMap.empty OrdMap.empty False

-- Process Entry -------------------------------------------------------------------------------------------------------

-- TODO - need to create typedata in ACF.module too

convertAST :: Module Id -> MExcept ACF.Module
convertAST (LitMod modData) = do
    ((_, freeIds'), fSt') <- runStateT (runSupplyT flatExprM [freeId ..]) initFlatState
    return $ ACF.Module (loopExprs fSt') (initExprs fSt') (head freeIds')
  where
    freeId = modFreeId modData
    flatExprM :: ConvM ()
    flatExprM = foldM_ convertTop () $ OrdMap.toList (modExprMap modData)
    initFlatState = mkFlatState (modTMap modData)

-- convert the toplet - we ensure that only TopLets with a single Id binding will exist at this point
-- (multiple ids (tuples) and TopType will have been already converted

-- can find the type here
convertTop :: () -> ([Id], AC.TopLet Id) -> ConvM ()
convertTop _ (id:[], AC.TopLet isInit t (id':[]) cE) = do
    assert (id == id') $ return ()
    -- set Init flag
    lift $ modify (\st -> st { inInit = isInit })
    fE <- convertExpr cE
    -- lookupt the type
    fT <- convertType <$> lookupType id
    insertExpr id fE fT

convertTop _ coreExpr = errorDump [MkSB coreExpr] "Cannot convert expression to CoreFlat" assert


-- convert the expression, this is straightforwad for the resticted Core AST we have now anyway
-- puts it into ANF too
convertExpr :: AC.Expr Id -> ConvM ACF.Expr
convertExpr e@(AC.Var (AC.LocalVar v) Nothing) = return $ ACF.Var (ACF.VarRef v)
-- directly store the nested let bindings within the flattened exprMap
-- TODO - what about multi-bindings? and state lets


-- can find the type here
convertExpr e@(AC.Let isInit t (b:[]) e1 e2) = do
    lift $ modify (\st -> st { inInit = isInit })
    fE1 <- convertExpr e1
    -- insert the binding using the given id as a toplet
    fT1 <- convertType <$> lookupType b
    insertExpr b fE1 fT1
    convertExpr e2

-- Literals
convertExpr e@(AC.Lit (AC.Num n U.NoUnit)) = return $ ACF.Var $ ACF.Num n
convertExpr e@(AC.Lit (AC.Boolean b)) = return $ ACF.Var $ ACF.Boolean b
convertExpr e@(AC.Lit (AC.Unit)) = return $ ACF.Var $ ACF.Unit

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
        let oldCurMap = curExprs st
        lift . put $ st { curExprs = OrdMap.empty }
        -- actuall convert the expression - returns the ret val
        e' <- convertExpr e
        -- create a dummy value to handle the returned value (as our Lets are top-level, rather than let e1 in e2)
        id <- supply
        st' <- lift $ get


        -- need to calc and convert the type here
        tMap <- curTMap <$> lift get
        fT <- convertType <$> (lift . lift $ calcTypeExpr tMap e)
        let es = OrdMap.insert id (ACF.ExprData e' fT) ( curExprs st')
        -- restore the old env
        lift . put $ st' { curExprs = oldCurMap }
        return es

-- Ode
convertExpr e@(AC.Ode (AC.LocalVar v) e1) = do
    v1 <- convertVar e1
    return $ ACF.Ode v v1

-- anything else,
convertExpr expr = errorDump [MkSB expr] "Cannot convert expression to CoreFlat" assert

-- Conversion Helper Functions -----------------------------------------------------------------------------------------

-- TODO - is this right?
-- should either lift/embed a var or convert an expr, create a new binding and return a refvar to it
convertVar :: AC.Expr Id -> ConvM ACF.Var
convertVar e = do
    case liftVarExpr e of
        Just var -> return $ var
        Nothing -> do
            -- convert the expression and return a var pointing to it
            id <- supply
            e' <- convertExpr e
            -- need to calc and convert the type here
            tMap <- curTMap <$> lift get
            fT <- convertType <$> (lift . lift $ calcTypeExpr tMap e)
            insertExpr id e' fT
            return $ ACF.VarRef id

-- | Performs single look-ahead into the expression and lifts to a Var expr if possible
liftVarExpr :: AC.Expr Id -> Maybe ACF.Var
liftVarExpr e@(AC.Lit (AC.Num n U.NoUnit)) = Just $ ACF.Num n
liftVarExpr e@(AC.Lit (AC.Boolean b)) = Just $ ACF.Boolean b
liftVarExpr e@(AC.Lit (AC.Unit)) = Just $ ACF.Unit
liftVarExpr e@(AC.Var (AC.LocalVar v) Nothing) = Just $ (ACF.VarRef v)
liftVarExpr e = Nothing

-- | Wrapper function to insert a given expression into the correct exprmap under a given Id
insertExpr :: Id -> ACF.Expr -> ACF.Type -> ConvM ()
insertExpr id fE fT = do
    -- id <- maybe supply return mId
    isInit <- inInit <$> lift get
    let exprData = ACF.ExprData fE fT
    case isInit of
        True    -> lift $ modify (\st -> st { initExprs = OrdMap.insert id exprData (initExprs st) })
        False   -> lift $ modify (\st -> st { loopExprs = OrdMap.insert id exprData (loopExprs st) })


-- | lookups and converts a Core type to a CoreFlat type
lookupType :: Id -> ConvM AC.Type
lookupType id = (Map.!) <$> (curTMap <$> lift get) <*> pure id

convertType :: AC.Type -> ACF.Type
convertType (AC.TBool)      = ACF.TBool
convertType (AC.TFloat _)   = ACF.TFloat
convertType (AC.TUnit)      = ACF.TUnit
