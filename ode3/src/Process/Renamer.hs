-----------------------------------------------------------------------------
--
-- Module      :  Core.Renamer
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Renamer takes a reordered AST and renames all variables into unique values (ints starting from 0),
-- thus it also deals with all scoping issues - as only are two scopes this should be fairly easy.
-- It also checks for all vlaue declarations, unused values, undefined values, and so on
-- Can, but currently doesn't, issue errors due to user defined model
--
-- Auxillary Checks Performed
-- * Referenced (local) binding does exist (within cur scope)
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Process.Renamer (
rename
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import qualified Data.Bimap as Bimap

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Data.Maybe (fromJust)
import Text.Printf (printf)

import qualified AST.Core as E
import qualified AST.Module as M
import Utils.CommonImports
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap

-- main types
type BindMap = Map.Map E.SrcId E.Id
-- type UnitMap = Map.Map E.Id E.UnitT

-- we need a supply of uniques, use monad supply again but with user-start param
type IdSupply = SupplyT E.Id (StateT BindMap MExcept)
-- type IdSupply = Supply E.Id

-- TODO - may need this monad when unify AST in order to process errors
--newtype IdSupply a = IdSupply { runIdSupply :: SupplyT Int (StateT ExprBinds (MExcept)) a }
--    deriving (Monad, MonadSupply Int)

-- data RenData a = RenData (E.Expr a) BindMap UnitMap

-- | Main rename function, takes a model bound by Ids and returns a single-scoped model bound by unique ints
rename :: M.Module E.DesId -> MExcept (M.Module E.Id)
rename (M.LitMod modData) = do
    (exprMap', topBinds, freeId) <-  renTop (M.modExprMap modData)
    return $ M.LitMod (updateModData modData exprMap' topBinds freeId)

rename (M.FunctorMod args modData) = do
    (exprMap', topBinds, freeId) <-  renTop (M.modExprMap modData)
    return $ M.FunctorMod args (updateModData modData exprMap' topBinds freeId)

-- | Update the module data with the next free id and calculate the idMap using the export set (then wipe it)
updateModData :: M.ModData E.DesId ->  M.ExprMap E.Id -> BindMap -> E.Id -> M.ModData E.Id
updateModData modData exprMap bMap freeId = modData { M.modExprMap = exprMap, M.modIdMap = idMap, M.modFreeId = freeId, M.modExportSet = Set.empty }
  where
    idMap = if Set.size (M.modExportSet modData) == 0
        then bMap -- export everything from top-level
        else Set.foldr (\srcId idMap -> Map.insert srcId (bMap ! srcId) idMap) Map.empty (M.modExportSet modData) -- fold over the export set and build the sigMap

-- |Need to build a conversion map of the top values first
renTop :: M.ExprMap E.DesId -> MExcept (M.ExprMap E.Id, BindMap, E.Id)
renTop exprMap = do
    ((exprMap', uniqs), topBinds) <- runStateT (runSupplyT exprMapM [0..]) Map.empty
    return $ (exprMap', topBinds, head uniqs)
  where
    exprMapM :: IdSupply (M.ExprMap E.Id)
    exprMapM = DF.foldlM convTopBind OrdMap.empty exprMap

    -- map over each expr, using the topmap, converting lets and building a new scopemap
    -- as traversing expr, as order is fixed this should be ok
    convTopBind :: M.ExprMap E.Id -> E.TopLet E.DesId -> IdSupply (M.ExprMap E.Id)
    convTopBind model (E.TopLet s t b e) = do
        -- store the bMap as modified within renExpr
        bMap <- lift $ get
        -- traverse over the expression, using initial bMap
        e' <- renExpr e
        -- convert the bindings, using orig bMap
        lift $ put bMap
        b' <- convBind b
        -- return the new bindmap and model
        return $ OrdMap.insert b' (E.TopLet s t b' e') model

    convTopBind model (E.TopType tName) = do
        b'@(tName':[]) <- convBind [tName]
        -- return the new bindmap and model
        return $ OrdMap.insert b' (E.TopType tName') model


-- | converts the bindings into unique ids and adds to the current, scoped bindmap
-- any additiona data, e.g. units defs, are added to the state monad
-- NOTE - this modifies the bMap
convBind :: E.BindList E.DesId -> IdSupply (E.BindList E.Id)
convBind bs = reverse <$> DF.foldlM f [] bs
  where
    -- helper func to convert an indiv binding
    f :: [E.Id] -> E.DesId -> IdSupply [E.Id]
    f bs' b = do
        map <- lift get
        -- get a unique id
        b' <- supply
        -- store the mapping
        lift . put $ Map.insert b b' map
        return (b':bs')


-- | Monadic binding lookup,
-- This should never throw an error (reorderer now catches all unknown variable references)
bLookup :: E.DesId -> IdSupply E.Id
bLookup v = do
    bMap <- lift get
    case (Map.lookup v bMap) of
        (Just v') -> return v'
        Nothing -> lift . lift . throwError $ printf "(RN01) Referenced variable %s not found in scope" (show v)

-- | Basic traverse over the expression structure - can't use helpers as map from (Expr a -> Expr b)
renExpr :: E.Expr E.DesId -> IdSupply (E.Expr E.Id)
-- need to check the expr and top bindings
renExpr (E.Var (E.LocalVar v) mRecId) = E.Var <$> (E.LocalVar <$> bLookup v) <*> pure mRecId

-- we don't rename module vars, least not yet - they are renamed during functor application
renExpr (E.Var (E.ModVar m v) mRecId) = return $ E.Var (E.ModVar m v) mRecId

-- same as above
renExpr (E.App (E.LocalVar b) e) = E.App <$> (E.LocalVar <$> bLookup b) <*> renExpr e

renExpr (E.App (E.ModVar m v) expr) = E.App <$> pure (E.ModVar m v) <*> renExpr expr

renExpr (E.Abs b expr) = do
    bMap <- lift $ get
    -- get the unique id for the arg and update the binding
    b' <- supply
    -- process the abs expr
    lift . put $ Map.insert b b' bMap
    expr' <- renExpr expr
    -- reset the bMap
    lift . put $ bMap
    return $ E.Abs b' expr'

-- need to create a new binding and keep processing
renExpr (E.Let s t b bExpr expr) = do
    bMap <- lift $ get
    -- process the binding bExpr with the existing bMap
    bExpr' <- renExpr bExpr
    -- get the unique ids and update the binding
    lift . put $ bMap
    b' <- convBind b
    -- process the main expr
    expr' <- renExpr expr
    -- reset the bMap
    lift . put $ bMap
    return $ E.Let s t b' bExpr' expr'

-- just traverse the structure
renExpr (E.Lit l) = return $ E.Lit l

renExpr (E.Op op expr) = E.Op op <$> renExpr expr

renExpr (E.If bExpr tExpr fExpr) = E.If <$> renExpr bExpr <*> renExpr tExpr <*> renExpr fExpr

-- need to map (or fold?) over the elements - map should be okay as a tuple should never create sub-bindings
renExpr (E.Tuple exprs) = E.Tuple <$> DT.mapM renExpr exprs

renExpr (E.Record nExprs) = E.Record <$> DT.mapM renExpr nExprs

renExpr (E.Ode (E.LocalVar v) eD) = E.Ode <$> (E.LocalVar <$> bLookup v) <*> renExpr eD

renExpr (E.Sde (E.LocalVar v) eW eD) = E.Sde <$> (E.LocalVar <$> bLookup v) <*> renExpr eW <*> renExpr eD

renExpr (E.Rre srcs dests eR) = E.Rre <$> rreLookup srcs <*> rreLookup dests  <*> renExpr eR
  where
    rreLookup = mapM (mapSndM (\(E.LocalVar vId) -> E.LocalVar <$> bLookup vId))

renExpr (E.TypeCast e tC ) = E.TypeCast <$> renExpr e <*> tC'
  where
    tC' = case tC of
        E.UnitCast u -> return $ E.UnitCast u
        E.WrapType (E.LocalVar t) -> E.WrapType <$> (E.LocalVar <$> bLookup t)
        E.WrapType (E.ModVar m v) -> return $ E.WrapType (E.ModVar m v)
        E.UnwrapType (E.LocalVar t) -> E.UnwrapType <$> (E.LocalVar <$> bLookup t)
        E.UnwrapType (E.ModVar m v) -> return $ E.UnwrapType (E.ModVar m v)

-- any unknown/unimplemented paths - not needed as match all
renExpr a = errorDump [MkSB a] "(RN) Unknown Core expression" assert
