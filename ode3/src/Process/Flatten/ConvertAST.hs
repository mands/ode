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

import qualified Utils.OrdMap as OrdMap
import qualified AST.Core as AC
import qualified AST.CoreFlat as ACF
import Utils.MonadSupply
import qualified Subsystem.Units as U


-- Types ---------------------------------------------------------------------------------------------------------------
-- Same monad as renamer
type IdSupply = SupplyT Id (StateT FlatState MExcept)
-- type IdSupply = Supply Id

instance Applicative IdSupply where
    pure = return
    (<*>) = ap

data FlatState = FlatState { curExprs :: ACF.ExprMap, loopExprs :: ACF.ExprMap,  initExprs :: ACF.ExprMap, inInit :: Bool } deriving (Show, Eq, Ord)
mkFlatState = FlatState OrdMap.empty OrdMap.empty OrdMap.empty False

-- Process Entry -------------------------------------------------------------------------------------------------------

convertAST :: Module Id -> MExcept ACF.Module
convertAST (LitMod exprMap modData) = do
    ((_, freeIds'), fSt') <- runStateT (runSupplyT flatExprM [freeId ..]) mkFlatState
    return $ ACF.Module (loopExprs fSt') (initExprs fSt') (head freeIds')
  where
    freeId = maybe 0 id $ modFreeId modData
    flatExprM :: IdSupply ()
    flatExprM = foldM_ convertTop () $ OrdMap.toList exprMap

-- convert the toplet - we ensure that only TopLets with a single Id binding will exist at this point
-- (multiple ids (tuples) and TopType will have been already converted
convertTop :: () -> ([Id], AC.TopLet Id) -> IdSupply ()
convertTop _ (id:[], AC.TopLet isInit (id':[]) cE) = do
    assert (id == id') $ return ()
    -- set Init flag
    lift $ modify (\st -> st { inInit = isInit })
    fE <- convertExpr cE
    insertExpr id fE

convertTop _ coreExpr = errorDump [MkSB coreExpr] "Cannot convert expression to CoreFlat" assert


-- convert the expression, this is straightforwad for the resticted Core AST we have now anyway
-- puts it into ANF too
convertExpr :: AC.Expr Id -> IdSupply ACF.Expr
convertExpr e@(AC.Var (AC.LocalVar v) Nothing) = return $ ACF.Var (ACF.VarRef v)
-- directly store the nested let bindings within the flattened exprMap
-- TODO - what about multi-bindings? and state lets
convertExpr e@(AC.Let isInit (b:[]) e1 e2) = do
    lift $ modify (\st -> st { inInit = isInit })
    e1' <- convertExpr e1
    -- insert the binding using the given id as a toplet
    insertExpr b e1'
    convertExpr e2

-- Literals
convertExpr e@(AC.Lit (AC.Num n U.NoUnit)) = return $ ACF.Var $ ACF.Num n
convertExpr e@(AC.Lit (AC.Num n _)) = return $ ACF.Var $ ACF.Num n -- TODO - remove me!!
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
        let es = OrdMap.insert id e' ( curExprs st')
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
convertVar :: AC.Expr Id -> IdSupply ACF.Var
convertVar e = do
    case liftVarExpr e of
        Just var -> return $ var
        Nothing -> do
            -- convert the expression and return a var pointing to it
            id <- supply
            e' <- convertExpr e
            insertExpr id e'
            return $ ACF.VarRef id

-- | Performs single look-ahead into the expression and lifts to a Var expr if possible
liftVarExpr :: AC.Expr Id -> Maybe ACF.Var
liftVarExpr e@(AC.Lit (AC.Num n U.NoUnit)) = Just $ ACF.Num n
liftVarExpr e@(AC.Lit (AC.Boolean b)) = Just $ ACF.Boolean b
liftVarExpr e@(AC.Lit (AC.Unit)) = Just $ ACF.Unit
liftVarExpr e@(AC.Var (AC.LocalVar v) Nothing) = Just $ (ACF.VarRef v)
liftVarExpr e = Nothing

-- | Wrapper function to insert a given expression into the correct exprmap under a given Id
insertExpr :: Id -> ACF.Expr -> IdSupply ()
insertExpr id fE = do
    -- id <- maybe supply return mId
    isInit <- inInit <$> lift get
    case isInit of
        True    -> lift $ modify (\st -> st { initExprs = OrdMap.insert id fE (initExprs st) })
        False   -> lift $ modify (\st -> st { loopExprs = OrdMap.insert id fE (loopExprs st) })


