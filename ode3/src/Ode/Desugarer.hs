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
-- |Desugarer - takes an Ode AST and desguars and converts into the Core langauge AST
-- Can issue errors due to user defined model
--
-----------------------------------------------------------------------------

module Ode.Desugarer (
desugar
) where

import qualified Data.Map as Map
import Utilities
import qualified Ode.AST as O
import qualified Core.AST as C

-- We need a supply of unique Ids
tmpName = "tmp"

-- |desugar function takes an ODE model representaiton and converts it into a lower-level Core AST
-- we only concern ourselves with single module models for now
desugar :: O.Model -> MExcept (C.Model C.Id)
desugar (O.Model files modules) = return a
  where
    -- fold over the list of components within the module creating the model
    a = foldl desugarModElems Map.empty testModElems

    -- filter to get complete modules and return only the elems list
    testModElems =  head . map (\(O.ModuleAbs _ _ elems) -> elems) . filter
                        (\m -> case m of
                            (O.ModuleAbs _ Nothing _) -> True
                            otherwise -> False)
                        $ modules


-- |desugar a top-level value constant(s)
-- need to thread thru our error and state monads
desugarModElems :: (C.Model C.Id) -> O.ModuleElem -> (C.Model C.Id)
desugarModElems map (O.ModuleElemValue (O.ValueDef ids value)) =
    if (isSingleElem ids)
        then Map.insert (singleElem ids) (C.TopLet (singleElem ids) (dsExpr value)) map
        else multMap'
  where
    -- call standard expression desugarer for the top-level let of a packed value
    multMap = Map.insert tmpName (C.TopLet tmpName (dsExpr value)) map
    -- now expand the pattern to create the list of sub-lets
    (multMap', _) = foldl createTopIds (multMap, 0) ids
    -- creates an Op that represnets the unpacking of a specific tuple value to a bound id
    createTopIds (map, n) id = (Map.insert tmpName (C.TopLet tmpName (C.Op (C.Unpack n) (C.Var tmpName))) map, n+1)

-- |desugar a top level component
desugarModElems map (O.ModuleElemComponent (O.Component name ins body outs)) = map'
  where
    map' = Map.insert name topAbs map
    topAbs = C.TopAbs name tmpName (desugarComp tmpName ins body outs)

-- | desugars and converts a component into a \c abstraction
-- not in tail-call form, could blow out the stack, but unlikely
desugarComp :: C.Id -> [O.Id] -> [O.CompStmt] -> [O.Expr] -> (C.Expr C.Id)
desugarComp name ins body outs = if (isSingleElem ins) then dsCompBody body else dsCompIns ins 0
  where
    -- unpack the input params, a custom fold over the multiple ins
    dsCompIns [] _ = dsCompBody body
    dsCompIns (x:xs) n = C.Let x (C.Op (C.Unpack n) (C.Var name)) (dsCompIns xs (n+1))

    -- process the body by pattern-matching on the statement types
    dsCompBody [] = dsCompOuts outs
    -- very similar to top-level value def - need to handle single elem diff to mult
    dsCompBody ((O.CompValue (O.ValueDef ids e)):xs) =
        if (isSingleElem ids)
            then C.Let (singleElem ids) (dsExpr e) (dsCompBody xs)
            else C.Let tmpName (dsExpr e) (dsCompBodyVals ids 0)
      where
        dsCompBodyVals [] _ = dsCompBody xs
        dsCompBodyVals (id:ids) n = C.Let id (C.Op (C.Unpack n) (C.Var tmpName)) (dsCompBodyVals ids (n+1))

    -- we ignore for now
    dsCompBody ((O.InitValueDef ids e):xs) = undefined
    dsCompBody ((O.OdeDef id init e):xs) = undefined
    dsCompBody ((O.RreDef id (from, to) e):xs) = undefined

    dsCompOuts outs = C.Tuple $ map dsExpr outs


-- |Expression desugarer - basically a big pattern amtch on all possible types
-- should prob enable warnings to pick up all unmatched patterns
dsExpr :: O.Expr -> C.Expr C.Id
dsExpr (O.UnExpr O.Not e) = C.Op C.Not $ dsExpr e

-- convert unary negation into (* -1)
dsExpr (O.UnExpr O.Neg e) = C.Op C.Mul $ C.Tuple [C.Lit (C.Num (-1)), dsExpr e]

dsExpr (O.BinExpr op a b) = C.Op (binOps op) $ C.Tuple [dsExpr a, dsExpr b]
dsExpr (O.Number n) = C.Lit (C.Num n)
dsExpr (O.NumSeq a b c) = C.Lit (C.NumSeq $ enumFromThenTo a b c)
dsExpr (O.ValueRef (O.LocalId id)) = C.Var id

-- create nested set of ifs for piecewise expression
dsExpr (O.Piecewise cases e) = dsIf cases
  where
    dsIf [] = dsExpr e
    dsIf ((testExpr, runExpr):xs) = C.If (dsExpr testExpr) (dsExpr runExpr) (dsIf xs)

-- convert call to a app, need to convert ins/args into a tuple first
dsExpr (O.Call (O.LocalId id) exprs) = C.App id arg
  where
    arg = if (isSingleElem exprs)
            then dsExpr $ singleElem exprs
            else C.Tuple $ map dsExpr exprs

-- any unknown/unimplemented paths - mainly modules for now
dsExpr _ = undefined

-- |Simple test to see if an expression contains only a single element or is a packed tuple
isSingleElem es = length es == 1
-- |Retrieve the single expression within a tuple, may cause exception
singleElem es = head es

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
