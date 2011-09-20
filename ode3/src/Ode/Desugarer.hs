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
import Control.Monad
import Control.Monad.Error as E
import Control.Monad.Trans
import Utils.Utils
import Utils.MonadSupply
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
desugar :: O.Model -> MExcept (C.Model C.SrcId)
desugar (O.Model files modules) = a
  where
    -- use the supply monad to generate unique names
    a = evalSupplyVars topMod

    -- fold over the list of components within the module creating the model
    topMod = foldM desugarModElems Map.empty testModElems

    -- filter to get complete modules and return only the elems list
    testModElems =  head . map (\(O.ModuleAbs _ _ elems) -> elems) . filter
                        (\m -> case m of
                            (O.ModuleAbs _ Nothing _) -> True
                            otherwise -> False)
                        $ modules

-- | desugar a top-level value constant(s)
desugarModElems :: (C.Model C.SrcId) -> O.ModuleElem -> TmpSupply (C.Model C.SrcId)
desugarModElems map (O.ModuleElemValue (O.ValueDef ids value)) =
--    if (isSingleElem ids)
--        then do
--            v' <- dsExpr value
--            let sB = C.SingleBind $ singleElem ids
--            return $ C.insert sB (C.TopLet sB v') map
--        else do
        do
            v' <- dsExpr value
            let mB = C.LetBind ids
            return $ C.insert mB (C.TopLet mB v') map


    -- call standard expression desugarer for the top-level let of a packed value
--            v' <- dsExpr value
--            tN <- supply
--            let multMap = C.insert tN (C.TopLet tN v') map
--            -- now expand the pattern to create the list of sub-lets
--            let (multMap', _, _) = foldl createTopIds (multMap, 0, tN) ids
--            return multMap'
--  where
--    -- creates an Op that represnets the unpacking of a specific tuple value to a bound id
--    createTopIds (map, n, tN) id = (C.insert id (C.TopLet id (C.Op (C.Unpack n) (C.Var tN))) map, n+1, tN)

-- | desugar a top level component
desugarModElems map (O.ModuleElemComponent (O.Component name ins body outs)) = do
    -- create a new tmpArg only if multiple elems
    arg <- if (isSingleElem ins) then return (singleElem ins) else supply
    --tmpBind <- supply
    v <- desugarComp arg ins body outs
    let topAbs = C.TopAbs (C.AbsBind name) arg v
    return $ C.insert (C.AbsBind name) topAbs map

-- | desugars and converts a component into a \c abstraction
-- not in tail-call form, could blow out the stack, but unlikely
desugarComp :: C.SrcId -> [O.Id] -> [O.CompStmt] -> [O.Expr] -> TmpSupply (C.Expr C.SrcId)
desugarComp name ins body outs = if (isSingleElem ins)
        then dsCompBody body -- error here for single elem
        else dsCompIns ins 0
  where
    -- unpack the input params, a custom fold over the multiple ins
    --dsCompIns [] _ = dsCompBody body
    -- dsCompIns (x:xs) n = liftM (C.Let x (C.Op (C.Unpack n) (C.Var name))) $ dsCompIns xs (n+1)
    dsCompIns bs _ = liftM (C.Let (C.LetBind bs) (C.Var name)) $ dsCompBody body

    -- process the body by pattern-matching on the statement types
    dsCompBody [] = dsCompOuts outs
    -- very similar to top-level value def - need to handle single elem diff to mult
    dsCompBody ((O.CompValue (O.ValueDef ids e)):xs) = liftM2 (C.Let (C.LetBind ids)) (dsExpr e) (dsCompBody xs)
--        if (isSingleElem ids)
--            then liftM2 (C.Let (C.SingleBind $ singleElem ids)) (dsExpr e) (dsCompBody xs)
--            else liftM2 (C.Let (C.MultiBind ids)) (dsExpr e) (dsCompBody xs)
--            else do
--                e' <- dsExpr e
--                tN <- supply
--                xs' <- dsCompBodyVals ids 0 tN
--                return $ C.Let tN e' xs'
--      where
--        dsCompBodyVals [] _ _ = dsCompBody xs
--        dsCompBodyVals (id:ids) n tN = do
--            ids' <- (dsCompBodyVals ids (n+1) tN)
--            return $ C.Let id (C.Op (C.Unpack n) (C.Var tN)) ids'

    -- we ignore for now
    dsCompBody ((O.InitValueDef ids e):xs) = lift $ throwError "test"
    dsCompBody ((O.OdeDef id init e):xs) = undefined
    dsCompBody ((O.RreDef id (from, to) e):xs) = undefined

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
dsExpr _ = undefined

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
