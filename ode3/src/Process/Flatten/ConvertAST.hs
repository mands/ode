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

-- Same monad as renamer
type IdSupply = SupplyT Int (StateT ACF.ExprMap MExcept)
-- type IdSupply = Supply Id

instance Applicative IdSupply where
    pure = return
    (<*>) = ap


convertAST :: Module Id -> MExcept ACF.Module
convertAST (LitMod exprMap modData) = do
    ((_, freeIds'), flatExprMap) <- runStateT (runSupplyT flatExprM [freeId ..]) OrdMap.empty
    return $ ACF.Module flatExprMap flatModData
  where
    flatModData = ACF.ModData "Hi!"
    freeId = maybe 0 id $ modFreeId modData

    flatExprM :: IdSupply ()
    flatExprM = foldM_ convertTop () $ OrdMap.toList exprMap

-- convert the toplet - we ensure that only TopLets with a single Id binding will exist at this point
-- (multiple ids (tuples) and TopType will have been already converted
convertTop :: () -> ([Id], AC.TopLet Id) -> IdSupply ()
convertTop _ (id:[], AC.TopLet _ _ coreExpr) = do
    flatExpr <- convertExpr coreExpr
    lift $ modify (\exprMap -> OrdMap.insert id flatExpr exprMap)

convertTop _ coreExpr = errorDump [MkSB coreExpr] "Cannot convert expression to CoreFlat" assert


-- convert the expression, this is straightforwad for the resticted Core AST we have now anyway
-- puts it into ANF too
convertExpr :: AC.Expr Id -> IdSupply ACF.Expr
convertExpr e@(AC.Var (AC.LocalVar v) Nothing) = return $ ACF.Var (ACF.VarRef v)
-- directly store the nested let bindings within the flattened exprMap
-- TODO - what about multi-bindings? and state lets
convertExpr e@(AC.Let _ (b:[]) e1 e2) = do
    e1' <- convertExpr e1
    -- insert the binding using the given id as a toplet
    lift $ modify (\exprMap -> OrdMap.insert b e1' exprMap)
    convertExpr e2

-- Literals
convertExpr e@(AC.Lit (AC.Num n U.NoUnit)) = return $ ACF.Var $ ACF.Num n
convertExpr e@(AC.Lit (AC.Num n _)) = return $ ACF.Var $ ACF.Num n -- TODO - remove me!!
convertExpr e@(AC.Lit (AC.Boolean b)) = return $ ACF.Var $ ACF.Boolean b
convertExpr e@(AC.Lit (AC.Unit)) = return $ ACF.Var $ ACF.Unit

-- Operators
-- multi-input op
convertExpr e@(AC.Op op (AC.Tuple es)) = do
    vs <- mapM liftOrInsert es
    return $ ACF.Op op vs
-- single-input Op
convertExpr e@(AC.Op op e1) = do
    v <- liftOrInsert e1
    return $ ACF.Op op [v]

-- If
-- TODO - is the ordering correct?
convertExpr e@(AC.If eB eT eF) = do
    vB <- liftOrInsert eB
    esT <- createSubExprs eT
    esF <- createSubExprs eF
    return $ ACF.If vB esT esF
  where
    -- convert the expression within its own env
    createSubExprs e = do
        -- save the old env
        oldMap <- lift $ get
        lift . put $ OrdMap.empty
        -- actuall convert the expression - returns the ret val
        e' <- convertExpr e
        -- create a dummy value to handle the returned value (as our Lets are top-level, rather than let e1 in e2)
        id <- supply
        es <- OrdMap.insert id e' <$> lift get
        -- restore the old env
        lift . put $ oldMap
        return es

-- Ode
convertExpr e@(AC.Ode (AC.LocalVar v) e1) = do
    v1 <- liftOrInsert e1
    return $ ACF.Ode v v1

-- anything else,
convertExpr expr = errorDump [MkSB expr] "Cannot convert expression to CoreFlat" assert

-- Conversion Helper Functions -----------------------------------------------------------------------------------------

-- TODO - is this right?
-- should either embed a var or create a new binding and return a refvar to it
liftOrInsert :: AC.Expr Id -> IdSupply ACF.Var
liftOrInsert e = do
    case liftVarExpr e of
        Just var -> return $ var
        Nothing -> do
            id  <- insertExpr e
            return $ ACF.VarRef id

-- | Wrapper function to convert and insert a given expression into the exprmap under a generated Id
insertExpr :: AC.Expr Id -> IdSupply Id
insertExpr e = do
    id <- supply
    e' <- convertExpr e
    lift $ modify (\exprMap -> OrdMap.insert id e' exprMap)
    return id

-- | Performs single look-ahead into the expression and lifts to a Var expr if possible
liftVarExpr :: AC.Expr Id -> Maybe ACF.Var
liftVarExpr e@(AC.Lit (AC.Num n U.NoUnit)) = Just $ ACF.Num n
liftVarExpr e@(AC.Lit (AC.Boolean b)) = Just $ ACF.Boolean b
liftVarExpr e@(AC.Lit (AC.Unit)) = Just $ ACF.Unit
liftVarExpr e@(AC.Var (AC.LocalVar v) Nothing) = Just $ (ACF.VarRef v)
liftVarExpr e = Nothing




