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
-- Can issue errors due to user defined model
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}

module Ode.Desugarer (
desugarMod
) where

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Data.Traversable as DT
import Data.List (nub)
import Debug.Trace
import Control.Applicative
import Control.Monad
import Control.Monad.Error as E
import Control.Monad.Trans
import Utils.Utils
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap
import qualified Ode.AST as O
import qualified Core.ExprAST as C
import qualified Core.ModuleAST as M

--type Id = C.SrcId

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


-- | desugar function takes an ODE module representaiton and converts it into a lower-level Core AST
-- use the supply monad to generate unique names
--desugarMod :: O.Module -> MExcept (C.TopMod Id)
--desugarMod (O.ModuleAbs name Nothing elems) =
--    evalSupplyVars $ (\(_, exprMap) -> C.TopMod name (C.LitMod exprMap modData)) <$> deRes
--  where
--    modData = C.ModuleData Map.empty Map.empty Bimap.empty Nothing
--    -- fold over the list of elems within the module creating the exprMap
--    deRes = (foldM desugarModElems CMod.emptySafeExprMap elems)
--
--desugarMod (O.ModuleAbs name (Just args) elems) =
--    evalSupplyVars $ (\(_, exprMap) -> C.TopMod name (C.FunctorMod args' exprMap modData)) <$> deRes
--  where
--    modData = C.ModuleData Map.empty Map.empty Bimap.empty Nothing
--    args' = OrdMap.fromList $ map (\arg -> (arg, Map.empty)) args
--    -- fold over the list of elems within the module creating the exprMap
--    deRes = foldM desugarModElems CMod.emptySafeExprMap elems
--
--desugarMod (O.ModuleApp name params) = return $ C.TopMod name (procParams params)
--  where
--    -- need to desugar into nested set of appMods and varMods
--    procParams (O.ModuleAppParams modId Nothing) = C.VarMod modId
--    procParams (O.ModuleAppParams funcId (Just args)) = C.AppMod funcId (map procParams args)

desugarMod :: [O.TopElem] -> MExcept (M.ExprMap C.SrcId)
desugarMod elems = evalSupplyVars $ snd <$> foldM desugarModElems M.emptySafeExprMap elems

-- | desugar a top-level value constant(s)
-- throws an error if a top-level is already defined
desugarModElems :: (M.SafeExprMap C.SrcId) -> O.TopElem -> TmpSupply (M.SafeExprMap C.SrcId)
desugarModElems sExprMap (O.TopElemValue (O.ValueDef ids value)) = do
    v' <- dsExpr value
    ids' <- DT.mapM subDontCares ids
    let mB = C.LetBind ids'
    lift $ M.insertTopExpr (C.TopLet mB v') sExprMap

-- | desugar a top level component
desugarModElems sExprMap (O.TopElemComponent (O.Component name ins outs body)) = do
    -- sub the ins
    ins' <- DT.mapM subDontCares ins
    -- create a new tmpArg only if multiple elems
    arg <- if (isSingleElem ins') then return (singleElem ins') else supply
    v <- desugarComp arg ins' outs body
    let topAbs = C.TopAbs (C.AbsBind name) arg v
    lift $ M.insertTopExpr topAbs sExprMap

-- | desugars and converts a component into a \c abstraction
-- not in tail-call form, could blow out the stack, but unlikely
desugarComp :: O.SrcId -> [O.SrcId] -> [O.Expr] -> [O.CompStmt] -> TmpSupply (C.Expr C.SrcId)
desugarComp argName [] outs body  = lift $ throwError ("(DS01) Component has zero inputs")
desugarComp argName ins [] body = lift $ throwError ("(DS02) Component has zero outputs")
desugarComp argName ins outs body = case ins of
                                    (singIn:[]) -> dsCompBody body
                                    _ | length ins == (length . nub) ins  -> dsCompIns ins
                                    _ | otherwise -> lift $ throwError ("(DS03) Component has inputs with the same name")
  where
    -- unpack the input params, a custom fold over the multiple ins
    dsCompIns bs = C.Let (C.LetBind bs) (C.Var (C.LocalVar argName)) <$> dsCompBody body

    -- process the body by pattern-matching on the statement types
    dsCompBody [] = dsCompOuts outs
    -- very similar to top-level value def - need to handle single elem diff to mult
    dsCompBody ((O.CompValue (O.ValueDef ids e)):xs) = do
        ids' <- DT.mapM subDontCares ids
        let mB = C.LetBind ids'
        C.Let mB <$> dsExpr e <*> dsCompBody xs

    -- TODO - we ignore for now
    dsCompBody ((O.InitValueDef ids e):xs) = lift $ throwError "test"
    dsCompBody ((O.OdeDef id init e):xs) = error "(DS) got ODE"
    dsCompBody ((O.RreDef id (from, to) e):xs) = error "(DS) got RRE"

    dsCompOuts outs = packElems outs

-- | Expression desugarer - basically a big pattern amtch on all possible types
-- should prob enable warnings to pick up all unmatched patterns
dsExpr :: O.Expr -> TmpSupply (C.Expr C.SrcId)
-- TODO - fix!
--dsExpr (O.UnExpr O.Not e) = liftM (C.Op C.Not) (dsExpr e)

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
dsExpr (O.ValueRef (O.LocalId id)) = return $ C.Var (C.LocalVar id)
dsExpr (O.ValueRef (O.ModId mId id)) = return $ C.Var (C.ModVar mId id)
dsExpr (O.Tuple exprs) = C.Tuple <$> DT.mapM dsExpr exprs


-- create nested set of ifs for piecewise expression
dsExpr (O.Piecewise cases e) = dsIf cases
  where
    dsIf [] = dsExpr e
    dsIf ((testExpr, runExpr):xs) = liftM3 C.If (dsExpr testExpr) (dsExpr runExpr) (dsIf xs)

-- convert call to a app, need to convert ins/args into a tuple first
dsExpr (O.Call (O.LocalId id) exprs) = liftM (C.App (C.LocalVar id)) $ packElems exprs
dsExpr (O.Call (O.ModId mId id) exprs) = liftM (C.App (C.ModVar mId id)) $ packElems exprs

-- any unknown/unimplemented paths - mainly modules for now
dsExpr a = trace (show a) (error "(DS) Unknown ODE3 expression")

-- |Simple test to see if an expression contains only a single element or is a packed tuple
isSingleElem es = length es == 1
-- |Retrieve the single expression within a tuple, may cause exception
singleElem es = head es

-- |packs up elements, if required, to use when calling a comp or setting up comp outputs
-- needed as \c-supports both single values and tuples
packElems es = if (isSingleElem es)
    then dsExpr $ singleElem es
    else liftM C.Tuple $ mapM dsExpr es

subDontCares :: O.ValId -> TmpSupply C.SrcId
subDontCares O.DontCare = supply
subDontCares (O.ValId i) = return i


-- |simple patttern matching convertor, boring but gotta be done...
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
