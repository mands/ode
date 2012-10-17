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
unpackTuples Module{..} = do
    -- run over both the init and loop exprs, using the same state
    ((initExprs', freeIds), st')   <- runStateT (runSupplyT (unpackM initExprs) [freeId..]) $ mkUnpackState
    ((loopExprs', freeIds'), _)    <- runStateT (runSupplyT (unpackM loopExprs) freeIds) $ st'

    return $ Module loopExprs' initExprs' (simOps) (head freeIds')
  where
    unpackM :: ExprMap -> UnpackM ExprMap
    unpackM exprMap = unpackTop (OrdMap.toList exprMap) OrdMap.empty


-- manual fold over ExprMap, as we need to control the fold, add expressions, etc.
unpackTop :: [(Id, ExprData)] -> ExprMap -> UnpackM ExprMap

-- last element is a tuple, we don't do anything in this case
unpackTop ((i, eD@(ExprData (Var (Tuple vs)) (TTuple ts))):[]) exprMap = return $ OrdMap.tailInsert i eD exprMap
-- last/return is anything else, unpack the expr and stop the loop
unpackTop ((i, eD):[]) exprMap = unpackExpr (i, eD) exprMap
-- empty element
unpackTop [] exprMap = return exprMap
-- unpack the expr and pass it on
unpackTop ((i, eD):es) exprMap = unpackTop es =<< unpackExpr (i, eD) exprMap


-- Actually unpack an expression
unpackExpr :: (Id, ExprData) -> ExprMap -> UnpackM ExprMap
-- tuple creation
unpackExpr (i, ExprData (Var (Tuple vs)) t) exprMap = do
    vs' <- mapM unpackVar vs
    -- tuple should already be unpacked, hence just need to build the refmap
    let refMap = foldl createTupleVar Map.empty $ zip [1..] vs'
    -- store the refMap, indexed by the tuple var id
    lift $ modify (\st -> st { unpackedIds = Map.insert i refMap (unpackedIds st) })
    -- copy the tuple accross
    return $ OrdMap.tailInsert i (ExprData (Var (Tuple vs')) t) exprMap
  where
    createTupleVar :: RefMap -> (Integer, Var) -> RefMap
    createTupleVar refMap (tupIdx, (VarRef v)) = Map.insert tupIdx v refMap

-- If - has it's own independent exprMaps for each branch
unpackExpr (i, ExprData (If vB emT emF) t) exprMap = do
    vB' <- unpackVar vB
    -- unpackExpr as it's own independent exprMaps for each branch
    emT' <- unpackTop (OrdMap.toList emT) OrdMap.empty
    emF' <- unpackTop (OrdMap.toList emF) OrdMap.empty
    -- add back the unpacked exprs
    -- continue the fold
    return $ OrdMap.tailInsert i (ExprData (If vB' emT' emF') t) exprMap

-- Var - we check for a ref to an already unpacked tuple
unpackExpr (i, eD@(ExprData (Var (VarRef vr)) (TTuple ts))) exprMap = do
    -- check if the ref has already been unpacked
    mRefMap <- Map.lookup vr <$> (unpackedIds <$> lift get)
    case mRefMap of
        Just refMap -> do
            -- store the cached refMap, indexed by var id
            lift $ modify (\st -> st { unpackedIds = Map.insert i refMap (unpackedIds st) })
            -- keep the var and move on
            return $ OrdMap.tailInsert i eD exprMap
        Nothing     -> return $ OrdMap.tailInsert i eD exprMap -- copy the var and move on

-- Other exprs
-- Var TupleRefs
unpackExpr (i, ExprData (Var v) t) exprMap = do
    v' <- unpackVar v
    return $ OrdMap.tailInsert i (ExprData (Var v') t) exprMap -- copy the var and move on

unpackExpr (i, ExprData (Op op vs) t) exprMap = do
    vs' <- mapM unpackVar vs
    return $ OrdMap.tailInsert i (ExprData (Op op vs') t) exprMap -- copy the var and move on


-- any other expr we just pass along (as no other nested exprs this should be ok)
unpackExpr (i, eD) exprMap = return $ OrdMap.tailInsert i eD exprMap -- copy the expr


-- | map over var that converts any tuplerefs to refs
unpackVar :: Var -> UnpackM Var
unpackVar var@(TupleRef vId tupIdx) = do
    mRefMap <- Map.lookup vId <$> (unpackedIds <$> lift get)
    return $ maybe var (\refMap -> (VarRef $ refMap Map.! tupIdx)) mRefMap
unpackVar var = return var
