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
updateModData modData bMap freeId = modData { M.modIdBimap = idBimap', M.modFreeId = Just freeId }
  where
    -- TODO - quick hack to convert
    idBimap = Bimap.fromList $ Map.toList bMap
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
    convTopBind (bMap, model) (E.TopLet s b e) = do
        -- convert the bindings
        (b', bMap') <- convBind b bMap
        -- traverse over the expression
        RenData expr' _ _ <- renExpr (RenData e bMap Map.empty)
        let model' = OrdMap.insert b' (E.TopLet s b' expr') model
        -- return the new bindmap and model
        return (bMap', model')


convBind :: E.Bind E.DesId -> BindMap -> IdSupply (E.Bind E.Id, BindMap)
convBind (E.Bind bs) map = liftM (mapFst (E.Bind . reverse)) $ DF.foldlM t ([], map) bs
  where
    t (bs', map) (b, _) = do
        b' <- supply
        let map' = Map.insert b b' map
        return (b':bs', map')


-- | Monadic binding lookup,
bLookup :: E.DesId -> BindMap -> IdSupply E.Id
bLookup (v, _) bMap =
    -- This should never throw an error (reorderer now catches all unknown variable references)
    -- maybe (renError ("Referenced variable " ++ v ++ " not found")) (\x -> return x) (Map.lookup v bMap)
    return $ maybe (error "(RNO1)") id $ (Map.lookup v bMap) -- trace' [MkSB v, MkSB bMap] "Scope Mappings"


data RenData a = RenData (E.Expr a) BindMap UnitMap

-- | Basic traverse over the expression structure - make into Data.Traversable
renExpr :: RenData E.DesId -> IdSupply (RenData E.Id)
-- renExpr :: RenData E.DesId -> BindMap -> E.Expr E.DesId -> IdSupply (BindMap, E.Expr E.Id)
-- need to check the expr and top bindings
renExpr (RenData (E.Var (E.LocalVar v)) bMap uMap) = do
    v' <- bLookup v bMap
    return $ RenData (E.Var (E.LocalVar v')) bMap uMap

-- we don't rename module vars, least not yet - they are renamed during functor application
renExpr rD@(RenData (E.Var (E.ModVar m v)) bMap uMap) = pure $ RenData (E.Var (E.ModVar m v)) bMap uMap

-- same as above
renExpr (RenData (E.App (E.LocalVar b) expr) bMap uMap) = do
    v' <- bLookup b bMap
    RenData expr' _ _ <- renExpr (RenData expr bMap uMap)
    return $ RenData (E.App (E.LocalVar v') expr') bMap uMap

renExpr (RenData (E.App (E.ModVar m v) expr) bMap uMap) = do
    RenData expr' _ _ <- renExpr (RenData expr bMap uMap)
    return $ RenData (E.App (E.ModVar m v) expr') bMap uMap

renExpr (RenData (E.Abs (b, _) expr) bMap uMap) = do
    -- get the unique id for the arg and update the binding
    b' <- supply
    let bMap' = Map.insert b b' bMap
    -- process the abs expr
    RenData expr' _ _ <- renExpr (RenData expr bMap uMap)
    return $ RenData (E.Abs b' expr') bMap uMap

-- need to create a new binding and keep processing
renExpr (RenData (E.Let s b bExpr expr) bMap uMap) = do
    -- process the binding bExpr with the existing bMap
    RenData bExpr' _ _ <- renExpr (RenData bExpr bMap uMap)
    -- get the unique ids and update the binding
    (b', bMap') <- convBind b bMap
    -- process the main expr
    RenData expr' _ _ <- renExpr (RenData expr bMap' uMap)
    return $ RenData (E.Let s b' bExpr' expr') bMap uMap

-- just traverse the structure
renExpr (RenData (E.Lit l) bMap uMap) = return $ (RenData (E.Lit l) bMap uMap)
renExpr (RenData (E.Op op expr) bMap uMap) =
    renExpr (RenData expr bMap uMap) >>= (\(RenData expr' _ _) -> return $ RenData (E.Op op expr') bMap uMap)

renExpr (RenData (E.If bExpr tExpr fExpr) bMap uMap) = do
    expr' <- E.If <$> apExpr bExpr <*> apExpr tExpr <*> apExpr fExpr
    return $ RenData expr' bMap uMap
  where
    apExpr :: E.Expr E.DesId -> IdSupply (E.Expr E.Id)
    apExpr expr = renExpr (RenData expr bMap uMap) >>= (\(RenData expr' _ _) -> return expr')

-- need to map (or fold?) over the elements - map should be okay as a tuple should never create sub-bindings
renExpr (RenData (E.Tuple exprs) bMap uMap) = do
    expr' <- E.Tuple <$> DT.mapM t exprs
    return $ RenData expr' bMap uMap
  where
    t e = renExpr (RenData e bMap uMap) >>= (\(RenData expr' _ _) -> return expr')

renExpr (RenData (E.Ode (E.LocalVar v) expr) bMap uMap) = do
    v' <- E.LocalVar <$> bLookup v bMap
    RenData expr' _ _ <- renExpr (RenData expr bMap uMap)
    return $ RenData (E.Ode v' expr') bMap uMap

renExpr (RenData (E.Rre (E.LocalVar src) (E.LocalVar dest) rate) bMap uMap) = do
    src' <- E.LocalVar <$> bLookup src bMap
    dest' <- E.LocalVar <$> bLookup dest bMap
    return $ RenData (E.Rre src' dest' rate) bMap uMap





