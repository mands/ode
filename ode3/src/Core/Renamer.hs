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
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances #-}

module Core.Renamer (
rename
) where

import qualified Data.Map as Map
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import qualified Data.Bimap as Bimap

import Debug.Trace -- love this shit!
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Data.Maybe (fromJust)

import qualified Core.ExprAST as E
import qualified Core.ModuleAST as M
import Utils.Utils
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap

-- we need a supply of uniques, use monad supply again but with user-start param
-- type IdSupply = SupplyT Int (State ExprBinds)
type IdSupply = Supply E.Id

-- TODO - may need this monad when unify AST in order to process errors
--newtype IdSupply a = IdSupply { runIdSupply :: SupplyT Int (StateT ExprBinds (MExcept)) a }
--    deriving (Monad, MonadSupply Int)

instance Applicative IdSupply where
    pure = return
    (<*>) = ap

-- main types
type BindMap = Map.Map E.SrcId E.Id
type UnitMap = Map.Map E.Id E.UnitT

-- | Main rename function, takes a model bound by Ids and returns a single-scoped model bound by unique ints
rename :: M.Module E.DesId -> MExcept (M.Module E.Id)
rename (M.LitMod exprMap modData) = trace ("(RN) " ++ (show res)) (Right res)
  where
    (exprMap', topBinds, freeId) = renTop exprMap
    modData' = updateModData modData topBinds freeId
    res = (M.LitMod exprMap' modData')

rename (M.FunctorMod args exprMap modData) = trace ("(RN) " ++ (show res)) (Right res)
  where
    (exprMap', topBinds, freeId) = renTop exprMap
    modData' = updateModData modData topBinds freeId
    res = (M.FunctorMod args exprMap' modData')

-- | Update the module data with the idBimap and next free id
updateModData :: M.ModuleData ->  BindMap -> E.Id -> M.ModuleData
updateModData modData tB freeId = modData { M.modIdBimap = idBimap', M.modFreeId = Just freeId }
  where
    -- TODO - quick hack to convert
    idBimap = Bimap.fromList $ Map.toList tB
    -- should never fail
    idBimap' = if (Bimap.valid idBimap) then idBimap else error "DUMP - invalid bimap!"

-- |Need to build a conversion map of the top values first
renTop :: M.ExprMap E.DesId -> (M.ExprMap E.Id, BindMap, E.Id)
renTop exprMap = (exprMap', topBinds, head uniqs)
  where
    ((topBinds, exprMap'), uniqs) = fromJust $ runSupply exprMapM [0..]
    exprMapM = DF.foldlM convTopBind (Map.empty, OrdMap.empty) exprMap

    -- map over each expr, using the topmap, converting lets and building a new scopemap
    -- as traversing expr, as order is fixed this should be ok
    convTopBind :: (BindMap, M.ExprMap E.Id) -> E.TopLet E.DesId -> IdSupply (BindMap, M.ExprMap E.Id)
    convTopBind (tB, model) (E.TopLet s b e) = do
        -- convert the bindings
        (b', tB') <- convBind b tB
        -- traverse over the expression
        (_, expr') <- renExpr tB e
        let model' = OrdMap.insert b' (E.TopLet s b' expr') model
        -- return the new bindmap and model
        return (tB', model')


convBind :: E.Bind E.DesId -> BindMap -> IdSupply (E.Bind E.Id, BindMap)
convBind (E.Bind bs) map = liftM (mapFst (E.Bind . reverse)) $ DF.foldlM t ([], map) bs
  where
    t (bs', map) (b, _) = do
        b' <- supply
        let map' = Map.insert b b' map
        return (b':bs', map')


-- | Monadic binding lookup,
bLookup :: E.DesId -> BindMap -> IdSupply E.Id
bLookup (v, _) tB =
    -- This should never throw an error (reorderer now catches all unknown variable references)
    -- maybe (renError ("Referenced variable " ++ v ++ " not found")) (\x -> return x) (Map.lookup v tB)
    return $ maybe (error "(RNO1)") id $ (Map.lookup v tB) -- trace' [MkSB v, MkSB tB] "Scope Mappings"


-- | Basic traverse over the expression structure - make into Data.Traversable
renExpr :: BindMap -> E.Expr E.DesId -> IdSupply (BindMap, E.Expr E.Id)
-- need to check the expr and top bindings
renExpr tB (E.Var (E.LocalVar v)) = pairM (pure tB) (E.Var <$> E.LocalVar <$> bLookup v tB)
-- we don't rename module vars, least not yet - they are renamed during functor application
renExpr tB (E.Var (E.ModVar m v)) = pairM (pure tB) $ pure (E.Var (E.ModVar m v))
-- same as above
renExpr tB (E.App (E.LocalVar b) expr) = do
    v' <- bLookup b tB
    (_, expr') <- renExpr tB expr
    return $ (tB, E.App (E.LocalVar v') expr')

renExpr tB (E.App (E.ModVar m v) expr) = pairM (pure tB) $ E.App <$> pure (E.ModVar m v) <*> expr'
  where
    expr' = snd <$> renExpr tB expr

renExpr tB (E.Abs (b, _) expr) = do
    -- get the unique id for the arg and update the binding
    b' <- supply
    let tB' = Map.insert b b' tB
    -- process the abs expr
    (_, expr') <- renExpr tB' expr
    return (tB, E.Abs b' expr')

-- need to create a new binding and keep processing
renExpr tB (E.Let s b bExpr expr) = do
    -- process the binding bExpr with the existing tB
    (_, bExpr') <- renExpr tB bExpr
    -- get the unique ids and update the binding
    (b', tB') <- convBind b tB
    -- process the main expr
    (_, expr') <- renExpr tB' expr
    return $ (tB, E.Let s b' bExpr' expr')

-- just traverse the structure
renExpr tB (E.Lit l) = pairM (pure tB) $ return (E.Lit l)
renExpr tB (E.Op op expr) = pairM (pure tB)  $ (E.Op op) <$> (snd <$> renExpr tB expr)
renExpr tB (E.If bExpr tExpr fExpr) = pairM (pure tB) $ E.If <$> re bExpr <*> re tExpr <*> re fExpr
  where
    re expr = snd <$> renExpr tB expr

-- need to map (or fold?) over the elements - map should be okay as a tuple should never create sub-bindings
renExpr tB (E.Tuple exprs) = pairM (pure tB) $ E.Tuple <$> DT.mapM (\e -> snd <$> renExpr tB e) exprs

renExpr tB (E.Ode (E.LocalVar v) expr) = do
    v' <- E.LocalVar <$> bLookup v tB
    (_, expr') <- renExpr tB expr
    return $ (tB, E.Ode v' expr')

renExpr tB (E.Rre (E.LocalVar src) (E.LocalVar dest) rate) = do
    src' <- E.LocalVar <$> bLookup src tB
    dest' <- E.LocalVar <$> bLookup dest tB
    return $ (tB, E.Rre src' dest' rate)





