-----------------------------------------------------------------------------
--
-- Module      :  Process.Flatten.UnpackTuples
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Unpack all tuples (and thus also records) into individual values on a CoreFlat AST
--
--
-----------------------------------------------------------------------------

module Process.Flatten.UnpackTuples (
unpackTuples
) where

import Control.Monad.State
import Utils.CommonImports

import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Traversable as DT
import qualified Utils.OrdMap as OrdMap

import Subsystem.SysState
import Utils.MonadSupply

import AST.Common
import AST.CoreFlat

import qualified Subsystem.Units as U
import qualified Subsystem.Types as T

-- Types ---------------------------------------------------------------------------------------------------------------
type UnpackM = SupplyT Id (StateT UnpackState MExcept)

type RefMap = Map.Map Integer Id -- holds a map from tuple references to varIds

data UnpackState = UnpackState  { unpackedIds :: Map.Map Id RefMap
                                -- , tupleAliases :: Map.Map Id Id -- we duplicate tuple-id-map entries for all aliases, as they will be static anyway
                                } deriving (Show, Eq, Ord)
mkUnpackState = UnpackState Map.empty -- Map.empty

-- Process Entry -------------------------------------------------------------------------------------------------------

unpackTuples :: Module -> MExcept Module
unpackTuples mod = do
    -- run over both the init and loop exprs, using the same state
    ((initEs', freeIds), st')   <- runStateT (runSupplyT (unpackM $ initExprs mod) [(freeId mod)..]) $ mkUnpackState
    ((loopEs', freeIds'), _)    <- runStateT (runSupplyT (unpackM $ loopExprs mod) freeIds) $ st'

    return $ Module loopEs' initEs' (head freeIds')
  where
    unpackM :: ExprMap -> UnpackM ExprMap
    unpackM exprMap = unpackTop (OrdMap.toList exprMap) OrdMap.empty


-- manual fold over ExprMap, as we need to control the fold, add expressions, etc.
unpackTop :: [(Id, ExprData)] -> ExprMap -> UnpackM ExprMap

-- tuple creation
unpackTop ((i, ExprData (Tuple vs) (TTuple ts)):es) exprMap = do
    -- unpack the tuple into separate vars
    (exprMap', refMap) <- DF.foldrM createTupleVar (exprMap, Map.empty) $ zip3 [1..] vs ts
    -- store the refMap,indexed by the tuple var id
    lift $ modify (\st -> st { unpackedIds = Map.insert i refMap (unpackedIds st) })
    -- now drop the tuple and move on
    unpackTop es $ exprMap'
  where
    createTupleVar :: (Integer, Var, Type) -> (ExprMap, RefMap) -> UnpackM (ExprMap, RefMap)
    createTupleVar (tupIdx, v, t) (exprMap, refMap) = do
        i <- supply
        let eD = ExprData (Var v) t
        return $ (OrdMap.headInsert i eD exprMap, Map.insert tupIdx i refMap)

-- If - has it's own independent exprMaps for each branch
unpackTop ((i, ExprData (If vB emT emF) t):es) exprMap = do
    emT' <- unpackTop (OrdMap.toList emT) OrdMap.empty
    emF' <- unpackTop (OrdMap.toList emF) OrdMap.empty
    -- continue the fold
    unpackTop es $ OrdMap.tailInsert i (ExprData (If vB emT' emF') t) exprMap


-- Var - we check for a ref to an already unpacked tuple
unpackTop ((i, eD@(ExprData (Var (VarRef vr)) (TTuple ts))):es) exprMap = do
    -- check if the ref has already been unpacked
    mRefMap <- Map.lookup vr <$> (unpackedIds <$> lift get)
    case mRefMap of
        Just refMap -> do
            -- store the cached refMap, indexed by var id
            lift $ modify (\st -> st { unpackedIds = Map.insert i refMap (unpackedIds st) })
            -- again drop the var and move on
            unpackTop es exprMap
        Nothing     -> unpackTop es $ OrdMap.tailInsert i eD exprMap -- copy the var and move on


-- Var TupleRef
unpackTop ((i, ExprData (Var var) t):es) exprMap = do
    var' <- unpackVar var
    unpackTop es $ OrdMap.tailInsert i (ExprData (Var var') t) exprMap -- copy the var and move on


-- any other expr we just pass along (as no other nested exprs this should be ok)
unpackTop ((i, eD):es) exprMap = unpackTop es $ OrdMap.tailInsert i eD exprMap

-- last/return element, we don't do anything in this case
unpackTop ((i, eD):[]) exprMap = return $ OrdMap.tailInsert i eD exprMap

-- empty element
unpackTop [] exprMap = return exprMap



-- map over var that converts any tuplerefs to refs
unpackVar :: Var -> UnpackM Var
unpackVar var@(TupleRef vId tupIdx) = do
    mRefMap <- Map.lookup vId <$> (unpackedIds <$> lift get)
    return $ maybe var (\refMap -> (VarRef $ refMap Map.! tupIdx)) mRefMap

unpackVar var = return var
