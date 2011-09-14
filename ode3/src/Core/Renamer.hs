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
import Debug.Trace -- love this shit!

import Control.Monad
import Control.Monad.State
import Control.Monad.Error
import Control.Applicative
import Data.Maybe (fromJust)

import qualified Core.AST as C
import Utils.Utils
import Utils.MonadSupply

-- we need a supply of uniques, use monad supply again but with user-start param
--newtype IntSupply a = IntSupply { runIntSupply :: SupplyT Int (StateT ExprBinds (MExcept)) a }
--    deriving (Monad, MonadSupply Int)
--type IntSupply = SupplyT Int (StateT ExprBinds (MExcept))
type IntSupply = SupplyT Int (State ExprBinds)

-- main types
newtype ExprBinds = ExprBinds (Map.Map C.Id Int) deriving Show
newtype TopBinds =  TopBinds (Map.Map C.Id Int) deriving Show

-- | Main rename function, takes a model bound by Ids and returns a single-scoped model bound by unique ints
-- I don't think this function can ever fail
-- eventually will take/return the higest bound int within the model
rename :: C.OrdModel C.Id -> MExcept (C.OrdModel Int)
rename cModel = trace (show renModel) (Right renModel)
  where
    renModel = renTop cModel

-- then map over each expr, using the topmap, converting lets and building a new scopemap
-- as traversing expr, as order is fixed this should be ok

convBind :: C.Bind C.Id -> Map.Map C.Id Int -> IntSupply (C.Bind Int, Map.Map C.Id Int)
convBind (C.SingleBind b) map = do
    b' <- supply
    let map' = Map.insert b b' map
    return (C.SingleBind b', map')

convBind (C.MultiBind bs) map = liftM (mapFst (C.MultiBind . reverse)) $ DF.foldlM t ([], map) bs
  where
    t (bs', map) b = do
        b' <- supply
        let map' = Map.insert b b' map
        return (b':bs', map')


-- |Need to build a conversion map of the top values first
renTop :: C.OrdModel C.Id -> (C.OrdModel Int)
renTop model = model'
  where
    ((_, model'), uniqs) = evalState (runSupplyT mModel [1..]) (ExprBinds Map.empty)
--renTop model = do
--    ((_, model'), uniqs) <- evalState (runSupplyT mModel [1..]) (ExprBinds Map.empty)
--    return model'

--model' -- topBinds

    m = (C.getOrdSeq model)

    mModel = DF.foldlM convTopBind (TopBinds Map.empty, C.empty) m -- (trace (show m) m)

    convTopBind :: (TopBinds, C.OrdModel Int) -> C.Top C.Id -> IntSupply (TopBinds, C.OrdModel Int)
    convTopBind (TopBinds map, model) (C.TopLet b e) = do
        (b', map') <- convBind b map
        --trace (show b') b'
        let map'' = TopBinds $ map'
        -- reset exprbinds
        lift $ put (ExprBinds Map.empty)
        -- should traverse over the expression here
        expr' <- renExpr map'' e
        let model' = C.insert b' (C.TopLet b' expr') model
        return (map'', model')

    convTopBind (TopBinds map, model) (C.TopAbs b arg e) = do
        (b', map') <- convBind b map
        --trace (show b') b'
        let map'' = TopBinds $ map'
        (arg', exprMap) <- convBind arg Map.empty
        --let map'' = Map.insert map'
        -- reset exprbinds
        lift $ put (ExprBinds $ exprMap)
        -- should traverse over the expression here
        expr' <- renExpr map'' e
        let model' = C.insert b' (C.TopAbs b' arg' expr') model
        return (map'', model')

-- | Basic traverse over the expression structure - make into Data.Traversable
renExpr :: TopBinds -> C.Expr C.Id -> IntSupply (C.Expr Int)

-- never called
--renExpr tB _ = undefined

-- need to check the expr and top bindings
renExpr tB (C.Var v) = liftM C.Var (bLookup v tB)

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

    -- get the unique ids
    (b', eB') <- convBind b eB

    -- add new binding
--    let eB' = Map.insert b b' eB
    -- put the new binding backend
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

-- | Monadic binding lookup,
-- TODO - refactor
bLookup :: C.Id -> TopBinds -> IntSupply Int
bLookup v (TopBinds tB) = do
    (ExprBinds eB) <- lift get
    let m = Map.lookup v eB -- trace (show v) (trace (show eB) (trace (show tB) (Map.lookup v eB)))
    case m of
        Just x -> return x
        -- This should never throw an error (reorderer now catches all unknown variable references)
        -- maybe (renError ("Referenced variable " ++ v ++ " not found")) (\x -> return x) (Map.lookup v tB)
        Nothing -> return $ tB Map.! v

--renError :: String -> IntSupply a
--renError s = lift . lift $ throwError s

-- PROB - these don't work, just use state monad instead
-- prob is the ExprBinds, which need to be carried and updating thru the bindings/threading
-- and the expression, which changes and thus must be passed as a parameter to the functions
-- monadic binding can be used to thread the ExprBinds, but not the expression
-- lift/ap can be used to pass in expression, but then would not thread/update the ExprBindings
--   instead it only threads whatever is held in the monad
-- SOL - could manually unpack/thread state using do-notation, easier just to use State monad

--rePairExpr ::
exprAp :: (Monad m) => (b -> c) -> m (a, b) -> m (a, c)
exprAp f m = m >>= (\(eB, e) -> return (eB, f e))

pairExpr :: ExprBinds -> IntSupply (C.Expr Int) -> IntSupply (ExprBinds, C.Expr Int)
--pairM :: UniqueIntSupply ExprBinds -> UniqueIntSupply (C.Expr Int) -> UniqueIntSupply (ExprBinds, C.Expr Int)
pairExpr a b = pairM (return a) b

pairM :: (Monad m) => m a -> m b -> m (a, b)
--pairM :: UniqueIntSupply ExprBinds -> UniqueIntSupply (C.Expr Int) -> UniqueIntSupply (ExprBinds, C.Expr Int)
pairM a b = liftM2 (,) a b

