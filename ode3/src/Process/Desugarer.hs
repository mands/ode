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

-- TODO
-- Fix all hard-coded NoUnits here, mainly SVals, Odes, & NumSeqs
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Process.Desugarer (
desugarOde, DesugarModData(..)
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error as E
import Control.Monad.Trans

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Bimap as Bimap
import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import Data.List (nub)
import Text.Printf(printf)

import Utils.CommonImports
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap
import AST.Common
import qualified AST.Ode as O
import qualified AST.Core as C
import qualified Subsystem.Units as U
import qualified AST.Module as M

-- We need a supply of unique Ids
-- supply type, transformed with Error/Except Monad
-- have to (lift throwError) as didn't create a newtype with auto-Deriving
type TmpSupply = SupplyT C.SrcId MExcept
throw x = lift $ E.throwError x

evalSupplyVars x = evalSupplyT x $ map (\x -> tmpPrefix ++ x) vars
  where
    vars = [replicate k ['A'..'Z'] | k <- [1..]] >>= sequence
    tmpPrefix = "_des"

instance Applicative TmpSupply where
    pure = return
    (<*>) = ap

data DesugarModData = DesugarModData    { mdE :: M.ExprList, mdQ :: U.Quantities, mdU :: [U.UnitDef]
                                        , mdI :: [ModImport], mdC :: [U.ConvDef]}

-- Desguar Top-Level Statements ----------------------------------------------------------------------------------------

desugarOde :: [O.OdeStmt] -> MExcept DesugarModData
desugarOde elems = do
    modData <- evalSupplyVars $ DF.foldlM desugarOde' (DesugarModData [] [] [] [] []) elems
    return $ updateModData modData
  where
    updateModData (DesugarModData e q u i c) = DesugarModData (reverse e) (reverse q) (reverse u) (reverse i) (reverse c)

    -- wrapper around the fold, matches on the top-level stmt type
    desugarOde' :: DesugarModData -> O.OdeStmt -> TmpSupply DesugarModData
    desugarOde' dsModData stmt@(O.ExprStmt exprStmt) = dsTopStmt (mdE dsModData) exprStmt >>= (\e1 -> return $ dsModData { mdE = e1 })

    -- filter the imports into their own list
    desugarOde' dsModData stmt@(O.ImportStmt imp) = return $ dsModData { mdI = imp:(mdI dsModData) }

    -- add quantity into the quantities assoc-list
    desugarOde' dsModData stmt@(O.QuantityStmt qName qDim) = return $ dsModData { mdQ = (qName, qDim):(mdQ dsModData) }

    -- create a list of Units, handle SI expansion here too
    -- can throw an error - maybe move this check into parser
    -- TODO -fix alias
    -- Base Unit Def
    desugarOde' dsModData stmt@(O.UnitStmt baseUnit (Just baseDimChar) mAlias isSI) = return $ dsModData { mdU=u', mdC=c' }
      where
        unitDef = U.UnitDef baseUnit $ U.getBaseDim baseDimChar
        (u', c') = if isSI then (siUnitDefs ++ unitDef:(mdU dsModData), siConvDefs ++ (mdC dsModData)) else (unitDef:(mdU dsModData), (mdC dsModData))
        (siUnitDefs, siConvDefs) = U.createSIs unitDef

    -- add the conv statments to the module metadata
    desugarOde' dsModData stmt@(O.ConvDefStmt fromUnit toUnit cExpr) = return $ dsModData { mdC = c' }
      where
        c' = U.ConvDef fromUnit toUnit cExpr:(mdC dsModData)

    -- add the type statments to exprList
    desugarOde' dsModData stmt@(O.TypeStmt typeName) = return $ dsModData { mdE = C.TopType typeName : (mdE dsModData) }

    -- desugarOde' st stmt@(O.UnitStmt baseUnits _ _ _) = throw $ printf "Found an invalid unit def %s" (show baseUnits)

-- Helper Functions ----------------------------------------------------------------------------------------------------

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
subDontCares :: O.BindId -> TmpSupply C.DesId
subDontCares O.DontCare = supply
subDontCares (O.BindId v) = return v


-- Desugar Main Ode Statements -----------------------------------------------------------------------------------------

-- | desugar a top-level value constant(s)
-- throws an error if a top-level is already defined
-- place in the front of the expr list, we reverse this later
dsTopStmt :: M.ExprList -> O.Stmt -> TmpSupply (M.ExprList)
dsTopStmt es stmt@(O.SValue _ _) = do
    (ids, expr) <- dsStmt stmt
    return $ C.TopLet True ids expr : es

--dsTopStmt es stmt@(O.OdeDef _ _ _) = do
--    (odeVar, odeExpr) <- dsStmt stmt
--    return $ C.TopLet False odeVar odeExpr : es
--
--dsTopStmt es stmt@(O.RreDef _ _  _) = do
--    (rreVar, rreExpr) <- dsStmt stmt
--    return $ C.TopLet False rreVar rreExpr : es
--
dsTopStmt es stmt = do
    (ids, expr) <- dsStmt stmt
    return $ C.TopLet False ids expr : es

-- desugar a nested let binding
dsNestedStmt :: [O.Stmt] -> O.Expr  -> TmpSupply (C.Expr C.DesId)
dsNestedStmt [] outs = dsExpr outs

dsNestedStmt (s@(O.Value _ _):xs) outs = do
    (ids, expr) <- dsStmt s
    C.Let False ids expr <$> dsNestedStmt xs outs

dsNestedStmt (s@(O.SValue _ _):xs) outs = do
    (ids, expr) <- dsStmt s
    C.Let True ids expr <$> dsNestedStmt xs outs

dsNestedStmt (s@(O.OdeDef _ _ _):xs) outs = do
    (odeVar, odeExpr) <- dsStmt s
    C.Let False odeVar odeExpr <$> dsNestedStmt xs outs

dsNestedStmt (s@(O.RreDef _ _ _):xs) outs = do
    (rreVar, rreExpr) <- dsStmt s
    C.Let False rreVar rreExpr <$> dsNestedStmt xs outs

dsNestedStmt (s@(O.Component _ _ _):xs) outs = do
    (ids, expr) <- dsStmt s
    C.Let False ids expr <$> dsNestedStmt xs outs

-- Main desugaring, called by top-level and nested level wrappers
dsStmt :: O.Stmt -> TmpSupply ([C.DesId], C.Expr C.DesId)
dsStmt (O.Value ids (body, value)) = do
    v' <- dsNestedStmt body value
    ids' <- DT.mapM subDontCares ids
    return $ (ids', v')

dsStmt (O.SValue ids expr) = do
    sExpr <- dsExpr expr
    return $ (ids, sExpr)

-- TODO - need to add the correct unit for the delta expr here
dsStmt (O.OdeDef initRef deltaName expr) = do
    odeExpr <- C.Ode <$> pure (C.LocalVar initRef) <*> dsExpr expr
    odeDeltaVar <- subDontCares deltaName
    return $ ([odeDeltaVar], odeExpr)

dsStmt (O.RreDef rate src dest) = do
    rreVar <- supply
    rreExpr <- C.Rre <$> pure (C.LocalVar src) <*> pure (C.LocalVar dest) <*> pure rate
    return $ ([rreVar], rreExpr)

-- desugar a top level component
dsStmt (O.Component name arg (body, out)) = do
    -- sub the ins
    arg' <- DT.mapM subDontCares arg
    -- create a new tmpArg only if multiple elems
    argName <- if (isSingleElem arg') then return (singleElem arg') else supply
    v <- desugarComp argName arg'
    return $ ([name], (C.Abs argName v))
  where
    -- | desugars and converts a component into a \c abstraction, not in tail-call form, could blow out the stack, but unlikely
    desugarComp :: O.SrcId -> [C.DesId] -> TmpSupply (C.Expr C.DesId)
    desugarComp _ (singArg:[]) = dsNestedStmt body out
    desugarComp argName ins = C.Let False ins (C.Var (C.LocalVar argName) Nothing) <$> dsNestedStmt body out

dsStmt stmt = throw $ printf "(DS01) Found an unhandled stmt that is top-level only - not nested \n%s" (show stmt)


-- Desugar Expressions -------------------------------------------------------------------------------------------------

-- | Expression desugarer - basically a big pattern amtch on all possible types
-- should prob enable warnings to pick up all unmatched patterns
dsExpr :: O.Expr -> TmpSupply (C.Expr C.DesId)
-- dsExpr (O.Op (Ops.BasicOp Ops.Not) (e:[])) = (C.Op (Ops.BasicOp Ops.Not)) <$> (dsExpr e)

-- convert unary negation into (* -1), with direct negate of nums
dsExpr (O.Op (BasicOp Neg) ((O.Number n u):[])) = dsExpr $ O.Number (negate n) u

-- use do-notation as more verbose/complex to sequence the tuple and lift
dsExpr (O.Op (BasicOp Neg) (e:[])) = do
    e' <- dsExpr e
    return $ C.Op (BasicOp Mul) (C.Tuple [C.Lit (C.Num (-1) U.NoUnit), e'])

dsExpr (O.Op op es) = C.Op op <$> packElems es

dsExpr (O.Number n Nothing) = return $ C.Lit (C.Num n U.NoUnit)
dsExpr (O.Number n (Just u)) = return $ C.Lit (C.Num n (U.mkUnit u))

dsExpr (O.NumSeq a b c) = return $ C.Lit (C.NumSeq (enumFromThenTo a b c) U.NoUnit)

dsExpr (O.Boolean b) = return $ C.Lit (C.Boolean b)
dsExpr (O.Time) = return $ C.Lit (C.Time)
dsExpr (O.None) = return $ C.Lit (C.Unit)
-- TODO - add record selection here
dsExpr (O.ValueRef (O.LocalId id) mRecId) = return $ C.Var (C.LocalVar id) mRecId
dsExpr (O.ValueRef (O.ModId modId id) mRecId) = return $ C.Var (C.ModVar (ModName modId) id) mRecId

-- we can convert from ext. tuple to int. record here
-- dsExpr (O.Tuple exprs) = C.Record . C.addLabels <$> DT.mapM dsExpr exprs
dsExpr (O.Tuple exprs) = C.Tuple <$> DT.mapM dsExpr exprs

-- desugar expr and convert from [(SrcId, Expr)] to Map.Map
-- need to also make sure all identifiers are unique
dsExpr (O.Record nExprs) = uniqIds >> C.Record <$> foldM insElem Map.empty nExprs
  where
    insElem recMap (id, e) = Map.insert <$> pure id <*> dsExpr e <*> pure recMap
    (ids, _) = unzip nExprs
    uniqIds = unless (listUniqs ids) $ throw $ printf "(DS02) - Record has duplicate identifies - %s" (show ids)

-- create nested set of ifs for piecewise expression
dsExpr (O.Piecewise cases e) = dsIf cases
  where
    dsIf [] = dsExpr e
    dsIf ((testExpr, runExpr):xs) = liftM3 C.If (dsExpr testExpr) (dsExpr runExpr) (dsIf xs)

-- convert call to a app, need to convert ins/args into a tuple first
dsExpr (O.Call (O.LocalId id) exprs) = C.App (C.LocalVar id) <$> packElems exprs
dsExpr (O.Call (O.ModId mId id) exprs) = C.App (C.ModVar (ModName mId) id) <$> packElems exprs

-- type experessions
dsExpr (O.ConvCast e u) = C.TypeCast <$> (dsExpr e) <*> pure (C.UnitCast $ U.mkUnit u)
dsExpr (O.WrapType e (O.LocalId id)) = C.TypeCast <$> (dsExpr e) <*> pure (C.WrapType (C.LocalVar id))
dsExpr (O.WrapType e (O.ModId modId id)) = C.TypeCast <$> (dsExpr e) <*> pure (C.WrapType (C.ModVar (ModName modId) id))
dsExpr (O.UnwrapType e (O.LocalId id)) = C.TypeCast <$> (dsExpr e) <*> pure (C.UnwrapType (C.LocalVar id))
dsExpr (O.UnwrapType e (O.ModId modId id)) = C.TypeCast <$> (dsExpr e) <*> pure (C.UnwrapType (C.ModVar (ModName modId) id))

-- any unknown/unimplemented paths - not needed as overlapping
dsExpr a = errorDump [MkSB a] "(DS) Unknown ODE3 expression" assert


---- | simple patttern matching convertor, boring but gotta be done...
--binOps :: O.BinOp -> C.Op
--binOps O.Add = C.Add
--binOps O.Sub = C.Sub
--binOps O.Mul = C.Mul
--binOps O.Div = C.Div
--binOps O.Mod = C.Mod
--binOps O.LT = C.LT
--binOps O.LE = C.LE
--binOps O.GT = C.GT
--binOps O.GE = C.GE
--binOps O.EQ = C.EQ
--binOps O.NEQ = C.NEQ
--binOps O.And = C.And
--binOps O.Or = C.Or
