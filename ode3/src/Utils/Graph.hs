-----------------------------------------------------------------------------
--
-- Module      :  Utils.Graph
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Helper functions for the `fgl' graph library (Data.Graph.Inductive)
--
-----------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction, DatatypeContexts #-}

module Utils.Graph (
GraphMap(..), mkGraphMap, runGraph, runGraph_, getEdgesFromPath, getNodeInt, nodeExists, insertNodeM_
) where

import Control.Monad.State
import Control.Conditional
import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.Tree -- TODO - maybe switch to PatriciaTree implementation?
import qualified Data.Graph.Inductive.NodeMap as NM

import Utils.Utils

-- ADT that holds the graph and nodemap (for now)
data (Ord a) => GraphMap a b = GraphMap { nodeMap :: NM.NodeMap a, graph :: Gr a b } deriving (Show)

mkGraphMap :: (Ord a) => GraphMap a b
mkGraphMap = GraphMap NM.new G.empty

-- wrappers around the NodeMapM state monad to reuse the existing nodemap
runGraph :: (Ord a) => GraphMap a b -> NM.NodeMapM a b Gr r -> (r, GraphMap a b)
runGraph (GraphMap nm g) m = mapSnd (uncurry GraphMap) $ runState m (nm, g)
--
runGraph_ :: (Ord a) => GraphMap a b -> NM.NodeMapM a b Gr r -> GraphMap a b
runGraph_ cg@(GraphMap nm g) m = snd $ runGraph cg m

--runGraph :: (Ord a) => GraphMap a b -> NM.NodeMapM a b Gr r -> (r, GraphMap a b)
--runGraph (GraphMap nm g) m = (r, GraphMap nm' g')
--  where
--    (r, (nm', g')) = NM.run g m
--
--runGraph_ :: (Ord a) => GraphMap a b -> NM.NodeMapM a b Gr r -> GraphMap a b
---- runGraph_ cg@(GraphMap nm g) m = snd $ runGraph cg m
--runGraph_ cg@(GraphMap nm g) m = GraphMap nm (NM.run_ g m)

getEdgesFromPath :: (G.LPath a) -> [a]
getEdgesFromPath (G.LP path) = map snd $ tail path

-- TODO - this is not correct in case of previously deleted nodes (acutally it may be, need investigate)
-- wrapper around the mkNode to lookup a node, assumes node already exists
-- getNode a :: NodeMap a -> a -> Int
getNodeInt (GraphMap nm g) n = if G.gelem nodeInt g then Just nodeInt else Nothing
  where
    nodeInt = fst . NM.mkNode_ nm $ n

-- nodeExists :: a -> NM.NodeMapM a b g Bool
nodeExists n = do
    (nm, g) <- get
    return $ maybe False (\_ -> True) $ getNodeInt (GraphMap nm g) n

-- | Special insert node that only inserts node if not already present
-- only needed for Tree-backed DynGraph, but PatriciaTree is okay
insertNodeM_ n = unlessM (nodeExists n) (void $ NM.insMapNodeM n)
