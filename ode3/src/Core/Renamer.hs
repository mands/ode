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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
type IntSupply = SupplyT Int (State ExprBinds)

-- TODO - may need this monad when unify AST in order to process errors
--newtype IntSupply a = IntSupply { runIntSupply :: SupplyT Int (StateT ExprBinds (MExcept)) a }
--    deriving (Monad, MonadSupply Int)

-- main types
newtype ExprBinds = ExprBinds (Map.Map E.SrcId E.Id) deriving Show
newtype TopBinds =  TopBinds (Map.Map E.SrcId E.Id) deriving Show

-- | Main rename function, takes a model bound by Ids and returns a single-scoped model bound by unique ints
rename :: M.Module E.SrcId -> MExcept (M.Module Int)
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
updateModData :: M.ModuleData ->  TopBinds -> Int -> M.ModuleData
updateModData modData (TopBinds map) freeId = modData { M.modIdBimap = idBimap', M.modFreeId = Just freeId }
  where
    -- TODO - quick hack to convert
    idBimap = Bimap.fromList $ Map.toList map
    -- should never fail
    idBimap' = if (Bimap.valid idBimap) then idBimap else error "DUMP - invalid bimap!"


convBind :: E.Bind E.SrcId -> Map.Map E.SrcId Int -> IntSupply (E.Bind Int, Map.Map E.SrcId Int)
--convBind (E.SingBind b) map = do
--    b' <- supply
--    let map' = Map.insert b b' map
--    return (E.SingBind b', map')

convBind (E.Bind bs) map = liftM (mapFst (E.Bind . reverse)) $ DF.foldlM t ([], map) bs
  where
    t (bs', map) b = do
        b' <- supply
        let map' = Map.insert b b' map
        return (b':bs', map')


-- TODO - refactor
-- | Monadic binding lookup,
bLookup :: E.SrcId -> TopBinds -> IntSupply Int
bLookup v (TopBinds tB) = do
    (ExprBinds eB) <- lift get
    let m = trace (mkTrace [show v, show eB, show tB]) $ Map.lookup v eB
    case m of
        Just x -> return x
        -- This should never throw an error (reorderer now catches all unknown variable references)
        -- maybe (renError ("Referenced variable " ++ v ++ " not found")) (\x -> return x) (Map.lookup v tB)
        Nothing -> return $ maybe (error "(RNO1)") id $ Map.lookup v tB

-- |Need to build a conversion map of the top values first
renTop :: M.ExprMap E.SrcId -> (M.ExprMap Int, TopBinds, Int)
renTop exprMap = (exprMap', topBinds, head uniqs)
  where
    ((topBinds, exprMap'), uniqs) = evalState (runSupplyT exprMapM [0..]) (ExprBinds Map.empty)
    exprMapM = DF.foldlM convTopBind (TopBinds Map.empty, OrdMap.empty) exprMap

    -- map over each expr, using the topmap, converting lets and building a new scopemap
    -- as traversing expr, as order is fixed this should be ok
    convTopBind :: (TopBinds, M.ExprMap Int) -> E.TopLet E.SrcId -> IntSupply (TopBinds, M.ExprMap Int)
    convTopBind (TopBinds map, model) (E.TopLet b e) = do
        (b', map') <- convBind b map
        let map'' = TopBinds $ map'
        -- reset exprbinds
        lift $ put (ExprBinds Map.empty)
        -- should traverse over the expression here
        expr' <- renExpr map'' e
        let model' = OrdMap.insert b' (E.TopLet b' expr') model
        return (map'', model')

-- | Basic traverse over the expression structure - make into Data.Traversable
renExpr :: TopBinds -> E.Expr E.SrcId -> IntSupply (E.Expr Int)
-- need to check the expr and top bindings
renExpr tB (E.Var (E.LocalVar v)) = (bLookup v tB) >>= (\v -> return $ E.Var (E.LocalVar v))
-- we don't rename module vars, least not yet - they are renamed during functor application
renExpr tB (E.Var (E.ModVar m v)) = return $ (E.Var (E.ModVar m v))
-- same as above
renExpr tB (E.App (E.LocalVar b) expr) = liftM2 E.App (liftM E.LocalVar v') expr'
  where
    v' = bLookup b tB
    expr' = renExpr tB expr

renExpr tB (E.App (E.ModVar m v) expr) = liftM2 E.App (return $ E.ModVar m v) expr'
  where
    expr' = renExpr tB expr

renExpr tB (E.Abs b expr) = do
    -- get the exprBinds
    (ExprBinds eB) <- lift get
    -- get the unique id for the arg and update the binding
    b' <- supply
    let eB' = Map.insert b b' eB
    -- put the new binding back
    lift $ put (ExprBinds eB')
    -- process the abs expr
    expr' <- renExpr tB expr
    return $ E.Abs b' expr'

-- need to create a new binding and keep processing
renExpr tB (E.Let b bExpr expr) = do
    -- process the binding bExpr with the existing eB
    bExpr' <- renExpr tB bExpr
    -- get the exprBinds
    (ExprBinds eB) <- lift get
    -- get the unique ids and update the binding
    (b', eB') <- convBind b eB
    -- put the new binding back
    lift $ put (ExprBinds eB')
    -- process the main expr
    expr' <- renExpr tB expr

    -- now put back the

    return $ E.Let b' bExpr' expr'

-- just traverse the structure
renExpr tB (E.Lit l) = return (E.Lit l)
renExpr tB (E.Op op expr) = liftM (E.Op op) (renExpr tB expr)
renExpr tB (E.If bExpr tExpr fExpr) = return E.If `ap` (re bExpr) `ap` (re tExpr) `ap` (re fExpr)
  where
    re = renExpr tB

-- need to map (or fold?) over the elements - map should be okay as a tuple should never create sub-bindings
renExpr tB (E.Tuple exprs) = liftM E.Tuple $ DT.mapM (\e -> renExpr tB e) exprs
