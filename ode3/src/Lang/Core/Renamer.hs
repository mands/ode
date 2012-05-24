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

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances #-}

module Lang.Core.Renamer (
rename
) where

import qualified Data.Map as Map
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import qualified Data.Bimap as Bimap

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Data.Maybe (fromJust)

import qualified Lang.Core.AST as E
import qualified Lang.Module.AST as M
import Utils.Utils
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap

-- main types
type BindMap = Map.Map E.SrcId E.Id
type UnitMap = Map.Map E.Id E.UnitT

-- we need a supply of uniques, use monad supply again but with user-start param
type IdSupply = SupplyT Int (State UnitMap)
-- type IdSupply = Supply E.Id

-- TODO - may need this monad when unify AST in order to process errors
--newtype IdSupply a = IdSupply { runIdSupply :: SupplyT Int (StateT ExprBinds (MExcept)) a }
--    deriving (Monad, MonadSupply Int)

instance Applicative IdSupply where
    pure = return
    (<*>) = ap

-- | Main rename function, takes a model bound by Ids and returns a single-scoped model bound by unique ints
rename :: (M.GlobalModEnv, M.Module E.DesId) -> MExcept (M.GlobalModEnv, M.Module E.Id)
rename (gModEnv, mod@(M.LitMod exprMap modData)) = trace' [MkSB (show mod')] "Renamer" (return $ (gModEnv, mod'))
  where
    (exprMap', topBinds, freeId) = renTop exprMap
    modData' = updateModData modData topBinds freeId
    mod' = (M.LitMod exprMap' modData')

rename (gModEnv, mod@(M.FunctorMod args exprMap modData)) = trace' [MkSB (show mod')] "Renamer" (return $ (gModEnv, mod'))
  where
    (exprMap', topBinds, freeId) = renTop exprMap
    modData' = updateModData modData topBinds freeId
    mod' = (M.FunctorMod args exprMap' modData')

-- | Update the module data with the idBimap and next free id
updateModData :: M.ModData ->  BindMap -> E.Id -> M.ModData
updateModData modData bMap freeId = modData { M.modIdBimap = idBimap', M.modFreeId = Just freeId }
  where
    -- TODO - quick hack to convert
    idBimap = Bimap.fromList $ Map.toList bMap
    -- should never fail
    idBimap' = if (Bimap.valid idBimap) then idBimap else errorDump [MkSB idBimap] "Invalid bimap!"

-- |Need to build a conversion map of the top values first
renTop :: M.ExprMap E.DesId -> (M.ExprMap E.Id, BindMap, E.Id)
renTop exprMap = trace' [MkSB uMap] "Final UnitMap" (exprMap', topBinds, head uniqs)
  where
    (((exprMap', topBinds), uniqs), uMap) = runState (runSupplyT exprMapM [0..]) (Map.empty)
    exprMapM = DF.foldlM convTopBind (OrdMap.empty, Map.empty) exprMap

    -- map over each expr, using the topmap, converting lets and building a new scopemap
    -- as traversing expr, as order is fixed this should be ok
    convTopBind :: (M.ExprMap E.Id, BindMap) -> E.TopLet E.DesId -> IdSupply (M.ExprMap E.Id, BindMap)
    convTopBind (model, bMap) (E.TopLet s b e) = do
        -- convert the bindings
        (b', bMap') <- convBind b bMap
        -- traverse over the expression
        (expr', _) <- renExpr e bMap
        let model' = OrdMap.insert b' (E.TopLet s b' expr') model
        -- return the new bindmap and model
        return (model', bMap')

-- | converts the bindings into unique ids and adds to the current, scoped bindmap
-- any additiona data, e.g. units defs, are added to the state monad
convBind :: E.Bind E.DesId -> BindMap -> IdSupply (E.Bind E.Id, BindMap)
convBind (E.Bind bs) map = liftM (mapFst (E.Bind . reverse)) $ DF.foldlM f ([], map) bs
  where
    -- helper func to convert an indiv binding
    f :: ([E.Id], BindMap) -> E.DesId -> IdSupply ([E.Id], BindMap)
    f (bs', map) (b, mUnit) = do
        -- get a unique id
        b' <- supply
        -- store the mapping
        let map' = Map.insert b b' map
        -- add any unit data
        _ <- case mUnit of
            Just unit -> do
                uMap <- lift get
                let uMap' = Map.insert b' unit uMap
                lift $ put uMap'
            Nothing -> return ()
        return (b':bs', map')


-- | Monadic binding lookup,
-- This should never throw an error (reorderer now catches all unknown variable references)
bLookup :: E.DesId -> BindMap -> IdSupply E.Id
bLookup (v, _) bMap = case (Map.lookup v bMap) of
    (Just v') -> return v'
    Nothing -> errorDump [MkSB v, MkSB bMap] "(RN01) Variable not found in scope"
    -- maybe (renError ("Referenced variable " ++ v ++ " not found")) (\x -> return x) (Map.lookup v bMap)

-- data RenData a = RenData (E.Expr a) BindMap UnitMap

-- combinator - apply the expr and throw away the bMap
apExpr :: E.Expr E.DesId -> BindMap -> IdSupply (E.Expr E.Id)
apExpr expr bMap = renExpr expr bMap >>= (\(expr', _) -> return expr')

-- | Basic traverse over the expression structure - make into Data.Traversable
renExpr :: E.Expr E.DesId -> BindMap -> IdSupply (E.Expr E.Id, BindMap)
-- need to check the expr and top bindings
renExpr (E.Var (E.LocalVar v)) bMap = pairM (E.Var <$> E.LocalVar <$> bLookup v bMap) (pure bMap)
-- we don't rename module vars, least not yet - they are renamed during functor application
renExpr (E.Var (E.ModVar m v)) bMap = return (E.Var (E.ModVar m v), bMap)
-- same as above
renExpr (E.App (E.LocalVar b) expr) bMap = do
    v' <- bLookup b bMap
    (expr', _) <- renExpr expr bMap
    return $ (E.App (E.LocalVar v') expr', bMap)

renExpr (E.App (E.ModVar m v) expr) bMap = pairM (E.App <$> pure (E.ModVar m v) <*> apExpr expr bMap) (pure bMap)

renExpr (E.Abs (b, _) expr) bMap = do
    -- get the unique id for the arg and update the binding
    b' <- supply
    let bMap' = Map.insert b b' bMap
    -- process the abs expr
    (expr', _) <- renExpr expr bMap'
    return (E.Abs b' expr', bMap)

-- need to create a new binding and keep processing
renExpr (E.Let s b bExpr expr) bMap = do
    -- process the binding bExpr with the existing bMap
    (bExpr', _) <- renExpr bExpr bMap
    -- get the unique ids and update the binding
    (b', bMap') <- convBind b bMap
    -- process the main expr
    (expr', _) <- renExpr expr bMap'
    return (E.Let s b' bExpr' expr', bMap)

-- just traverse the structure
renExpr (E.Lit l) bMap = pairM (pure $ E.Lit l) (pure bMap)
renExpr (E.Op op expr) bMap = pairM ((E.Op op) <$> apExpr expr bMap) $ pure bMap
renExpr (E.If bExpr tExpr fExpr) bMap = pairM ifExpr $ pure bMap
  where
    ifExpr = E.If <$> apExpr bExpr bMap <*> apExpr tExpr bMap <*> apExpr fExpr bMap

-- need to map (or fold?) over the elements - map should be okay as a tuple should never create sub-bindings
renExpr (E.Tuple exprs) bMap = pairM tExpr $ pure bMap
  where
    tExpr = E.Tuple <$> DT.mapM (\e -> apExpr e bMap) exprs

renExpr (E.Ode (E.LocalVar v) expr) bMap = do
    v' <- E.LocalVar <$> bLookup v bMap
    (expr', _) <- renExpr expr bMap
    return $ (E.Ode v' expr', bMap)

renExpr (E.Rre (E.LocalVar src) (E.LocalVar dest) rate) bMap = do
    src' <- E.LocalVar <$> bLookup src bMap
    dest' <- E.LocalVar <$> bLookup dest bMap
    return $ (E.Rre src' dest' rate, bMap)
