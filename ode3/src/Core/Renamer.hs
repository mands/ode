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
-- | Renamer takes a reordered AST and renames all variables into unique values, thus it also deals with all scoping
-- issues. As only are two scopes this should be fairly easy.
-- It also checks for all vlaue declarations, unused values, undefined values, and so on
-- Can issue errors due to user defined model
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

import qualified Core.AST as C
import Utils.Utils
import Utils.MonadSupply
import qualified Utils.OrdMap as OrdMap

-- we need a supply of uniques, use monad supply again but with user-start param
--newtype IntSupply a = IntSupply { runIntSupply :: SupplyT Int (StateT ExprBinds (MExcept)) a }
--    deriving (Monad, MonadSupply Int)
--type IntSupply = SupplyT Int (StateT ExprBinds (MExcept))
type IntSupply = SupplyT Int (State ExprBinds)

-- main types
newtype ExprBinds = ExprBinds (Map.Map C.SrcId C.Id) deriving Show
newtype TopBinds =  TopBinds (Map.Map C.SrcId C.Id) deriving Show

-- TODO - store idBimap and maxId in the ModuleData
-- | Main rename function, takes a model bound by Ids and returns a single-scoped model bound by unique ints
-- I don't think this function can ever fail
rename :: C.Module C.SrcId -> MExcept (C.Module Int)
rename (C.LitMod exprMap modData) = trace (show exprMap') (Right (C.LitMod exprMap' modData'))
  where
    (exprMap', topBinds, freeId) = renTop exprMap
    modData' = updateModData modData topBinds freeId

rename (C.FunctorMod args exprMap modData) = trace (show exprMap') (Right (C.FunctorMod args exprMap' modData'))
  where
    (exprMap', topBinds, freeId) = renTop exprMap
    modData' = updateModData modData topBinds freeId


-- | Update the module data with the idBimap and next free id
updateModData :: C.ModuleData ->  TopBinds -> Int -> C.ModuleData
updateModData modData (TopBinds map) freeId = modData { C.modIdBimap = idBimap', C.modFreeId = Just freeId }
  where
    -- TODO - quick hack to convert
    idBimap = Bimap.fromList $ Map.toList map
    -- should never fail
    idBimap' = if (Bimap.valid idBimap) then idBimap else error "DUMP - invalid bimap!"


convBind :: C.Bind C.SrcId -> Map.Map C.SrcId Int -> IntSupply (C.Bind Int, Map.Map C.SrcId Int)
convBind (C.AbsBind b) map = do
    b' <- supply
    let map' = Map.insert b b' map
    return (C.AbsBind b', map')

convBind (C.LetBind bs) map = liftM (mapFst (C.LetBind . reverse)) $ DF.foldlM t ([], map) bs
  where
    t (bs', map) b = do
        b' <- supply
        let map' = Map.insert b b' map
        return (b':bs', map')


-- TODO - refactor
-- | Monadic binding lookup,
bLookup :: C.SrcId -> TopBinds -> IntSupply Int
bLookup v (TopBinds tB) = do
    (ExprBinds eB) <- lift get
    let m = Map.lookup v eB
    case m of
        Just x -> return x
        -- This should never throw an error (reorderer now catches all unknown variable references)
        -- maybe (renError ("Referenced variable " ++ v ++ " not found")) (\x -> return x) (Map.lookup v tB)
        Nothing -> return $ tB Map.! v

-- |Need to build a conversion map of the top values first
renTop :: C.ExprMap C.SrcId -> (C.ExprMap Int, TopBinds, Int)
renTop exprMap = (exprMap', topBinds, head uniqs)
  where
    ((topBinds, exprMap'), uniqs) = evalState (runSupplyT exprMapM [1..]) (ExprBinds Map.empty)
--    ((_, model'), uniqs) <- evalState (runSupplyT mModel [1..]) (ExprBinds Map.empty)

    exprMapM = DF.foldlM convTopBind (TopBinds Map.empty, OrdMap.empty) exprMap

    -- map over each expr, using the topmap, converting lets and building a new scopemap
    -- as traversing expr, as order is fixed this should be ok
    convTopBind :: (TopBinds, C.ExprMap Int) -> C.Top C.SrcId -> IntSupply (TopBinds, C.ExprMap Int)
    convTopBind (TopBinds map, model) (C.TopLet b e) = do
        (b', map') <- convBind b map
        let map'' = TopBinds $ map'
        -- reset exprbinds
        lift $ put (ExprBinds Map.empty)
        -- should traverse over the expression here
        expr' <- renExpr map'' e
        let model' = OrdMap.insert b' (C.TopLet b' expr') model
        return (map'', model')

    convTopBind (TopBinds map, model) (C.TopAbs b arg e) = do
        (b', map') <- convBind b map
        let map'' = TopBinds $ map'
        arg' <- supply
        let exprMap = Map.singleton arg arg'
        -- reset exprbinds
        lift $ put (ExprBinds $ exprMap)
        -- should traverse over the expression here
        expr' <- renExpr map'' e
        let model' = OrdMap.insert b' (C.TopAbs b' arg' expr') model
        return (map'', model')


-- | Basic traverse over the expression structure - make into Data.Traversable
renExpr :: TopBinds -> C.Expr C.SrcId -> IntSupply (C.Expr Int)

-- need to check the expr and top bindings
renExpr tB (C.Var (C.LocalVar v)) = (bLookup v tB) >>= (\v -> return $ C.Var (C.LocalVar v))
renExpr tB (C.Var (C.ModVar m v)) = return $ (C.Var (C.ModVar m v))
-- same as above
renExpr tB (C.App b expr) = liftM2 C.App v' expr'
  where
    v' = bLookup b tB
    expr' = renExpr tB expr

-- need to create a new binding and keep processing
renExpr tB (C.Let b bExpr expr) = do
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
    return $ C.Let b' bExpr' expr'

-- just traverse the structure
renExpr tB (C.Lit l) = return (C.Lit l)

renExpr tB (C.Op op expr) = liftM (C.Op op) (renExpr tB expr)

renExpr tB (C.If bExpr tExpr fExpr) = return C.If `ap` (re bExpr) `ap` (re tExpr) `ap` (re fExpr)
  where
    re = renExpr tB

-- need to map (or fold?) over the elements - map should be okay as a tuple should never create sub-bindings
renExpr tB (C.Tuple exprs) = liftM C.Tuple $ DT.mapM (\e -> renExpr tB e) exprs

--renError :: String -> IntSupply a
--renError s = lift . lift $ throwError s

-- PROB - these don't work, just use state monad instead
-- prob is the ExprBinds, which need to be carried and updating thru the bindings/threading
-- and the expression, which changes and thus must be passed as a parameter to the functions
-- monadic binding can be used to thread the ExprBinds, but not the expression
-- lift/ap can be used to pass in expression, but then would not thread/update the ExprBindings
--   instead it only threads whatever is held in the monad
-- SOL - could manually unpack/thread state using do-notation, easier just to use State monad

exprAp :: (Monad m) => (b -> c) -> m (a, b) -> m (a, c)
exprAp f m = m >>= (\(eB, e) -> return (eB, f e))

pairExpr :: ExprBinds -> IntSupply (C.Expr Int) -> IntSupply (ExprBinds, C.Expr Int)
pairExpr a b = pairM (return a) b
