-----------------------------------------------------------------------------
--
-- Module      :  Ode.Desugarer
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Desugarer - takes an Ode AST and desugars/converts into the Core AST
-- Can issue errors due to user defined model, however it shouldn't, desugaring should be
-- deterministic, always convertiable to Core, and errors are checked there
-- however, due to bindings we need
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}

module Lang.Ode.Desugarer (
desugarMod
) where

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import Data.List (nub)
import Debug.Trace
import Control.Applicative
import Control.Monad
import Control.Monad.Error as E
import Control.Monad.Trans
import Utils.Utils
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap
import qualified Lang.Ode.AST as O
import qualified Lang.Core.AST as C
import qualified Lang.Module.AST as M

-- We need a supply of unique Ids
-- supply type, transformed with Error/Except Monad
-- have to (lift throwError) as didn't create a newtype with auto-Deriving
type TmpSupply = SupplyT C.SrcId MExcept
throw x = lift $ E.throwError x

evalSupplyVars x = evalSupplyT x $ map (\x -> tmpPrefix ++ x) vars
  where
    vars = [replicate k ['A'..'Z'] | k <- [1..]] >>= sequence
    tmpPrefix = "des"

instance Applicative TmpSupply where
    pure = return
    (<*>) = ap


desugarMod :: [O.Stmt] -> MExcept M.ExprList
desugarMod elems = liftM reverse $ evalSupplyVars $ DF.foldlM desugarTopStmt [] elems

-- | desugar a top-level value constant(s)
-- throws an error if a top-level is already defined
desugarTopStmt :: M.ExprList -> O.Stmt -> TmpSupply (M.ExprList)
desugarTopStmt es stmt@(O.SValue _ _) = do
    (ids, expr) <- desugarStmt stmt
    return $ C.TopLet True (C.Bind ids) expr : es

desugarTopStmt es stmt@(O.OdeDef (O.ValId name unit) init _) = do
    (odeVar, odeExpr) <- desugarStmt stmt
    let initExpr = C.Lit (C.Num init)
    let es' = C.TopLet True (C.Bind [(name, unit)]) initExpr : es
    return $ C.TopLet False (C.Bind odeVar) odeExpr : es'

desugarTopStmt es stmt@(O.RreDef _ _  _) = do
    (rreVar, rreExpr) <- desugarStmt stmt
    return $ C.TopLet False (C.Bind rreVar) rreExpr : es

desugarTopStmt es stmt = do
    (ids, expr) <- desugarStmt stmt
    return $ C.TopLet False (C.Bind ids) expr : es

-- desugar a nested let binding
desugarS' :: [O.Stmt] -> O.Expr  -> TmpSupply (C.Expr C.DesId)
desugarS' [] outs = dsExpr outs
desugarS' (s@(O.Value _ _ _):xs) outs = do
    (ids, expr) <- desugarStmt s
    C.Let False (C.Bind ids) expr <$> desugarS' xs outs

desugarS' (s@(O.SValue _ _):xs) outs = do
    (ids, expr) <- desugarStmt s
    C.Let True (C.Bind ids) expr <$> desugarS' xs outs

desugarS' (s@(O.OdeDef (O.ValId name unit) init _):xs) outs = do
    (odeVar, odeExpr) <- desugarStmt s
    let subExpr' = C.Let False (C.Bind odeVar) odeExpr <$> desugarS' xs outs
    let initExpr = C.Lit (C.Num init)
    C.Let True (C.Bind [(name, unit)]) <$> pure initExpr <*> subExpr'

desugarS' (s@(O.RreDef _ _ _):xs) outs = do
    (rreVar, rreExpr) <- desugarStmt s
    C.Let False (C.Bind rreVar) rreExpr <$> desugarS' xs outs

desugarS' (s@(O.Component _ _ _ _):xs) outs = do
    (ids, expr) <- desugarStmt s
    C.Let False (C.Bind ids) expr <$> desugarS' xs outs


-- Main desugaring
desugarStmt :: O.Stmt -> TmpSupply ([C.DesId], C.Expr C.DesId)
desugarStmt (O.Value ids value body) = do
    v' <- desugarS' body value
    ids' <- DT.mapM subDontCares ids
    return $ (ids', v')

desugarStmt (O.SValue ids values) = do
    let vs' = map (C.Lit . C.Num) values
    let vs'' = case vs' of
                    v:[] -> v
                    _ -> C.Tuple vs'
    ids' <- DT.mapM subDontCares ids
    return $ (ids', vs'')

-- TODO - need to add the correct unit for the delta expr here
desugarStmt (O.OdeDef (O.ValId name unit) init expr) = do
    odeExpr <- C.Ode <$> pure (C.LocalVar (name, unit)) <*> dsExpr expr
    odeVar <- supply
    return $ ([(odeVar, Nothing)], odeExpr)

desugarStmt (O.RreDef rate src dest) = do
    rreVar <- supply
    rreExpr <- C.Rre <$> pure (C.LocalVar (src, Nothing)) <*> pure (C.LocalVar (dest, Nothing)) <*> pure rate
    return $ ([(rreVar, Nothing)], rreExpr)

-- | desugar a top level component
desugarStmt (O.Component name ins outs body) = do
    -- sub the ins
    ins' <- DT.mapM subDontCares ins
    -- create a new tmpArg only if multiple elems
    argName <- if (isSingleElem ins') then return (fst $ singleElem ins') else supply
    v <- desugarComp argName ins'
    return $ ([(name, Nothing)], (C.Abs (argName, Nothing) v))
  where
    -- | desugars and converts a component into a \c abstraction, not in tail-call form, could blow out the stack, but unlikely
    desugarComp :: O.SrcId -> [C.DesId] -> TmpSupply (C.Expr C.DesId)
    desugarComp _ (singIn:[]) = desugarS' body outs
    desugarComp argName ins = C.Let False (C.Bind ins) (C.Var (C.LocalVar (argName, Nothing))) <$> desugarS' body outs

--error (show s)


--desugarModElems :: O.Stmt -> TmpSupply (C.TopLet C.SrcId)
--desugarModElems (O.StmtValue (O.Value ids value body)) = do
--    v' <- desugarCompStmts body value
--    ids' <- DT.mapM subDontCares ids
--    return $ C.TopLet (C.Bind ids') v'
--
---- | desugar a top level component
--desugarModElems (O.StmtComponent (O.Component name ins outs body)) = do
--    -- sub the ins
--    ins' <- DT.mapM subDontCares ins
--    -- create a new tmpArg only if multiple elems
--    arg <- if (isSingleElem ins') then return (singleElem ins') else supply
--    v <- desugarComp arg ins'
--    return $ C.TopLet (C.Bind [name]) (C.Abs arg v)
--  where
--    -- | desugars and converts a component into a \c abstraction, not in tail-call form, could blow out the stack, but unlikely
--    desugarComp :: O.SrcId -> [O.SrcId] -> TmpSupply (C.Expr C.SrcId)
--    desugarComp argName (singIn:[]) = desugarCompStmts body outs
--    desugarComp argName ins = C.Let (C.Bind ins) (C.Var (C.LocalVar argName)) <$> desugarCompStmts body outs
--
--
--desugarCompStmts :: [O.Stmt] -> O.Expr  -> TmpSupply (C.Expr C.SrcId)
---- process the body by pattern-matching on the statement types
--desugarCompStmts [] outs = dsExpr outs
---- very similar to top-level value def
--desugarCompStmts ((O.StmtValue (O.Value ids e vOuts)):xs) outs = do
--        ids' <- DT.mapM subDontCares ids
--        let mB = C.Bind ids'
--        C.Let mB <$> desugarCompStmts vOuts e <*> desugarCompStmts xs outs

-- TODO - we ignore for nowddd
--desugarCompStmts ((O.InitValueDef ids e):xs) _ = lift $ throwError "test"
--desugarCompStmts ((O.OdeDef id init e):xs) _ = error "(DS) got ODE"
--desugarCompStmts ((O.RreDef id (from, to) e):xs) _ = error "(DS) got RRE"

-- | Expression desugarer - basically a big pattern amtch on all possible types
-- should prob enable warnings to pick up all unmatched patterns
dsExpr :: O.Expr -> TmpSupply (C.Expr C.DesId)
dsExpr (O.UnExpr O.Not e) = liftM (C.Op C.Not) (dsExpr e)

-- convert unary negation into (* -1)
dsExpr (O.UnExpr O.Neg e) = do
    e' <- dsExpr e
    return $ C.Op C.Mul (C.Tuple [C.Lit (C.Num (-1)), e'])

-- use do-notation as more verbose/complex to sequence the tuple and lift
dsExpr (O.BinExpr op a b) = do
    a' <- dsExpr a
    b' <- dsExpr b
    return $ C.Op (binOps op) (C.Tuple [a', b'])

dsExpr (O.Number n) = return $ C.Lit (C.Num n)
dsExpr (O.NumSeq a b c) = return $ C.Lit (C.NumSeq $ enumFromThenTo a b c)
dsExpr (O.Boolean b) = return $ C.Lit (C.Boolean b)
dsExpr (O.Time) = return $ C.Lit (C.Time)
dsExpr (O.Unit) = return $ C.Lit (C.Unit)
dsExpr (O.ValueRef (O.LocalId id)) = return $ C.Var (C.LocalVar (id, Nothing))
dsExpr (O.ValueRef (O.ModId mId id)) = return $ C.Var (C.ModVar mId id)
dsExpr (O.Tuple exprs) = C.Tuple <$> DT.mapM dsExpr exprs

-- create nested set of ifs for piecewise expression
dsExpr (O.Piecewise cases e) = dsIf cases
  where
    dsIf [] = dsExpr e
    dsIf ((testExpr, runExpr):xs) = liftM3 C.If (dsExpr testExpr) (dsExpr runExpr) (dsIf xs)

-- convert call to a app, need to convert ins/args into a tuple first
dsExpr (O.Call (O.LocalId id) exprs) = liftM (C.App (C.LocalVar (id, Nothing))) $ packElems exprs
dsExpr (O.Call (O.ModId mId id) exprs) = liftM (C.App (C.ModVar mId id)) $ packElems exprs

-- any unknown/unimplemented paths - not needed as match all
-- dsExpr a = trace (show a) (error "(DS) Unknown ODE3 expression")

-- | Simple test to see if an expression contains only a single element or is a packed tuple
isSingleElem es = length es == 1
-- | Retrieve the single expression within a tuple, may cause exception
singleElem es = head es

-- | packs up elements, if required, to use when calling a comp or setting up comp outputs
-- needed as \c-supports both single values and tuples
packElems es = if (isSingleElem es)
    then dsExpr $ singleElem es
    else liftM C.Tuple $ mapM dsExpr es

-- | creates new unique variable identifiers for all don't care values
subDontCares :: O.ValId -> TmpSupply C.DesId
subDontCares O.DontCare = (,) <$> supply <*> pure Nothing
subDontCares (O.ValId v u) = return (v, u)

-- | simple patttern matching convertor, boring but gotta be done...
binOps :: O.BinOp -> C.Op
binOps O.Add = C.Add
binOps O.Sub = C.Sub
binOps O.Mul = C.Mul
binOps O.Div = C.Div
binOps O.Mod = C.Mod
binOps O.LT = C.LT
binOps O.LE = C.LE
binOps O.GT = C.GT
binOps O.GE = C.GE
binOps O.EQ = C.EQ
binOps O.NEQ = C.NEQ
binOps O.And = C.And
binOps O.Or = C.Or
