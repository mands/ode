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
-- | Desugarer - takes an Ode AST and desguars and converts into the Core langauge AST
-- Can issue errors due to user defined model
--
-----------------------------------------------------------------------------

module Ode.Desugarer (
desugar
) where

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import Control.Monad
import Control.Monad.Error as E
import Control.Monad.Trans
import Utils.Utils
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap
import qualified Ode.AST as O
import qualified Core.AST as C

-- We need a supply of unique Ids
-- supply type, transformed with Error/Except Monad
-- have to (lift throwError) as didn't create a newtype with auto-Deriving
type TmpSupply = SupplyT C.SrcId MExcept
throw x = lift $ E.throwError x

evalSupplyVars x = evalSupplyT x $ map (\x -> tmpPrefix ++ x) vars
  where
    vars = [replicate k ['A'..'Z'] | k <- [1..]] >>= sequence
    tmpPrefix = "des"

--tmpName = "tmpName"
-- | desugar function takes an ODE model representaiton and converts it into a lower-level Core AST
-- we only concern ourselves with single module models for now
desugar :: O.Model -> MExcept [C.Module C.SrcId]
desugar (O.Model _ modules) = a
  where
    -- use the supply monad to generate unique names
    a = evalSupplyVars testMods

    -- filter to get complete modules and return only the elems list
    testMods = mapM desugarMod . filter
                        (\m -> case m of
                            (O.ModuleAbs _ Nothing _) -> True
                            otherwise -> False)
                        $ modules


desugarMod :: O.Module -> TmpSupply (C.Module C.SrcId)
desugarMod (O.ModuleAbs name Nothing elems) = do
    -- fold over the list of elems within the module creating the exprMap
    exprMap <- foldM desugarModElems OrdMap.empty elems
    return $ C.VarMod name exprMap (C.ModuleData Map.empty Bimap.empty Nothing)

desugarMod (O.ModuleAbs name (Just args) elems) = undefined -- C.AbsMod name elems (C.ModuleData Map.empty Bimap.empty Nothing))
desugarMod (O.ModuleApp  name _) = undefined -- C.AppMod name elems (C.ModuleData Map.empty Bimap.empty Nothing))


-- | desugar a top-level value constant(s)
desugarModElems :: (C.ExprMap C.SrcId) -> O.ModuleElem -> TmpSupply (C.ExprMap C.SrcId)
desugarModElems exprMap (O.ModuleElemValue (O.ValueDef ids value)) = do
    v' <- dsExpr value
    let mB = C.LetBind ids
    return $ OrdMap.insert mB (C.TopLet mB v') exprMap

-- | desugar a top level component
desugarModElems exprMap (O.ModuleElemComponent (O.Component name ins body outs)) = do
    -- create a new tmpArg only if multiple elems
    arg <- if (isSingleElem ins) then return (singleElem ins) else supply
    v <- desugarComp arg ins body outs
    let topAbs = C.TopAbs (C.AbsBind name) arg v
    return $ OrdMap.insert (C.AbsBind name) topAbs exprMap

-- | desugars and converts a component into a \c abstraction
-- not in tail-call form, could blow out the stack, but unlikely
desugarComp :: C.SrcId -> [O.Id] -> [O.CompStmt] -> [O.Expr] -> TmpSupply (C.Expr C.SrcId)
desugarComp name ins body outs = if (isSingleElem ins)
        then dsCompBody body -- error here for single elem
        else dsCompIns ins 0
  where
    -- unpack the input params, a custom fold over the multiple ins
    dsCompIns bs _ = liftM (C.Let (C.LetBind bs) (C.Var name)) $ dsCompBody body

    -- process the body by pattern-matching on the statement types
    dsCompBody [] = dsCompOuts outs
    -- very similar to top-level value def - need to handle single elem diff to mult
    dsCompBody ((O.CompValue (O.ValueDef ids e)):xs) = liftM2 (C.Let (C.LetBind ids)) (dsExpr e) (dsCompBody xs)

    -- TODO - we ignore for now
    dsCompBody ((O.InitValueDef ids e):xs) = lift $ throwError "test"
    dsCompBody ((O.OdeDef id init e):xs) = error "got ODE"
    dsCompBody ((O.RreDef id (from, to) e):xs) = error "got RRE"

    dsCompOuts outs = packElems outs

-- | Expression desugarer - basically a big pattern amtch on all possible types
-- should prob enable warnings to pick up all unmatched patterns
dsExpr :: O.Expr -> TmpSupply (C.Expr C.SrcId)
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
dsExpr (O.ValueRef (O.LocalId id)) = return $ C.Var id

-- create nested set of ifs for piecewise expression
dsExpr (O.Piecewise cases e) = dsIf cases
  where
    dsIf [] = dsExpr e
    dsIf ((testExpr, runExpr):xs) = liftM3 C.If (dsExpr testExpr) (dsExpr runExpr) (dsIf xs)

-- convert call to a app, need to convert ins/args into a tuple first
dsExpr (O.Call (O.LocalId id) exprs) = liftM (C.App id) $ packElems exprs

-- any unknown/unimplemented paths - mainly modules for now
dsExpr _ = error "Unknown ODE expression to desugar"

-- |Simple test to see if an expression contains only a single element or is a packed tuple
isSingleElem es = length es == 1
-- |Retrieve the single expression within a tuple, may cause exception
singleElem es = head es

-- |packs up elements, if required, to use when calling a comp or setting up comp outputs
-- needed as \c-supports both single values and tuples
packElems es = if (isSingleElem es)
    then dsExpr $ singleElem es
    else liftM C.Tuple $ mapM dsExpr es

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
