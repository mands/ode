-----------------------------------------------------------------------------
--
-- Module      :  Utils.OrdMap
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | An OrdMap of type b is a container that allows for quick lookups whilst presevering the order of insertion of elements
-- into the strucutre
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies  #-}

module Utils.OrdMap (
OrdMap, (!), lookup, member, empty, singleton, insert, delete, update, elems, keys, map, foldl, union,
toList, fromList, mapAccum, toMap,

) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import qualified Data.Functor as Functor
import Control.Applicative (liftA, liftA2, pure, (<$>), (<*>))
import Data.Maybe (fromJust, isJust)
import Prelude hiding (foldl, lookup, map)

type AssocList k v = [(k, v)]

-- use an Assoclist implementation for now
-- most functions cribbed from Data.Map
-- add more as needed, only basic essential funcs here, rest acessible via toList/fromList
-- need to make instance of Traversable and Foldable
newtype OrdMap k v = OrdMapC [(k,v)] deriving (Show, Eq, Ord)

(!) :: (Ord k) => OrdMap k v -> k -> v
(!) (OrdMapC m) k = fromJust $ List.lookup k m

lookup :: (Ord k) => k -> OrdMap k v -> Maybe v
lookup k (OrdMapC m) = List.lookup k m

member :: (Ord k) => k -> OrdMap k v -> Bool
member k (OrdMapC m) = isJust $ List.lookup k m

empty :: OrdMap k v
empty = OrdMapC []

singleton :: k -> v -> OrdMap k v
singleton k v = OrdMapC [(k,v)]

-- if already exsists use existing insertion order, else add at the tail of list
insert :: (Ord k) => k -> v -> OrdMap k v -> OrdMap k v
insert k v (OrdMapC m) = OrdMapC $ maybe insert' update' mInd
  where
    update' i = let (hd', tl') = splitAt i m
                in hd' ++ ((k, v) : (tail tl'))
    insert' = m ++ [(k,v)]
    mInd = List.elemIndex k (keys (OrdMapC m))

delete :: (Ord k) => k -> OrdMap k v -> OrdMap k v
delete k (OrdMapC m) = OrdMapC $ maybe m delete' mInd
  where
    mInd = List.elemIndex k (keys (OrdMapC m))
    delete' i = let (hd', tl') = splitAt i m
                in hd' ++ (tail tl')

update :: (Ord k) => (v -> Maybe v) -> k -> OrdMap k v -> OrdMap k v
update f b m = if member b m then update' (f (m!b)) else m
  where
    update' (Just v) = insert b v m
    update' Nothing = delete b m

elems :: OrdMap k v -> [v]
elems (OrdMapC m) = List.map snd m

keys :: OrdMap k v -> [k]
keys (OrdMapC m) = List.map fst m

toList :: OrdMap k v -> [(k,v)]
toList (OrdMapC m) = m

fromList :: [(k,v)] -> OrdMap k v
fromList = OrdMapC

toMap :: (Ord k) => OrdMap k v -> Map.Map k v
toMap = Map.fromList . toList

-- map over the keys and values - preserves the ordering regardless
map :: ((k, v) -> (k', v')) -> OrdMap k v -> OrdMap k' v'
map f (OrdMapC m) = OrdMapC $ List.map f m

foldl :: (a -> (k,v) -> a) -> a -> OrdMap k v -> a
foldl f z (OrdMapC m) = List.foldl f z m

union :: (Eq k) => OrdMap k v -> OrdMap k v -> OrdMap k v
union (OrdMapC a) (OrdMapC b) = OrdMapC $ List.unionBy bindingEq a b
  where
    bindingEq (kA, _) (kB, _) = kA == kB


-- other useful functions
mapAccum :: (Ord k) => (a -> b -> (a, c)) -> a -> OrdMap k b -> (a, OrdMap k c)
mapAccum f z m = foldl f' (z, empty) m
  where
    f' (acc, m) (k, v) = let (acc', v') = f acc v in (acc', insert k v' m)

-- |Basic instances
-- this can't be done as the contained object within OrdModel is not the same as that within the sequence (i.e. b vs Top b)
-- instead need to get the seq directly to fold over

-- Functor instance - only applies to the values
instance Functor (OrdMap k) where
    -- need to fmap over the sequence, then recreate the orig structure using insert with a fold
    fmap f (OrdMapC m) = OrdMapC $ fmap (\(k, v) -> (k, f v)) m

instance DF.Foldable (OrdMap k) where
    --foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f z (OrdMapC m) = DF.foldr (\(k, v) s -> f v s) z m

instance DT.Traversable (OrdMap k) where
    -- like mapM?
    traverse f (OrdMapC m) = OrdMapC <$> DT.traverse f' m
      where
        f' (k, v) = (,) <$> (pure k) <*> (f v)

-- Optimised version using a Map and Seq
--
----data OrdModel b = OrdModel { ordMap :: IntMap.IntMap b, ordSeq :: Seq.Seq b }
--data IOrdModel b = IOrdModel { ordMap :: Map.Map (Bind b) Int, ordSeq :: Seq.Seq (Top b) } deriving Show
--
---- Newtype test - is it needed in this case - type classes should be enough with escape hatch if neccessary
--newtype OrdModel b = OrdModel { getOrdModel :: IOrdModel b } deriving Show
--getOrdMap :: OrdModel b -> Map.Map (Bind b) Int
--getOrdMap om = ordMap . getOrdModel $ om
--
--putOrdMap :: Map.Map (Bind b) Int -> OrdModel b -> OrdModel b
--putOrdMap map m = OrdModel $ IOrdModel map (getOrdSeq m)
--
--getOrdSeq :: OrdModel b -> Seq.Seq (Top b)
--getOrdSeq om = ordSeq . getOrdModel $ om
--
--putOrdSeq :: Seq.Seq (Top b) -> OrdModel b -> OrdModel b
--putOrdSeq seq m = OrdModel $ IOrdModel (getOrdMap m) seq
--
---- | Optimised OrdModel that uses both a map and a seq to hold the ordering of elements
--instance ModelMap (OrdModel b) b where
--    (!) m b = Seq.index (getOrdSeq m) ((getOrdMap m) Map.! b)
--    lookup b m = Map.lookup b (getOrdMap m) >>= (\i -> return $ Seq.index (getOrdSeq m) i)
--    member b m = Map.member b (getOrdMap m)
--    empty = OrdModel $ IOrdModel Map.empty Seq.empty
--    singleton b v = OrdModel $ IOrdModel (Map.singleton b 0) (Seq.singleton v)
--    insert b v m = OrdModel $ IOrdModel map' seq'
--      where
--        map' = if member b m then map else Map.insert b (Seq.length seq) map
--        seq' = if member b m -- if already in model
--            then Seq.update (map Map.! b) v seq  -- then update in cur pos
--            else seq Seq.|> v -- else append to end of seq
--        map = getOrdMap m
--        seq = getOrdSeq m
--
--    delete b m = OrdModel $ IOrdModel map' seq'
--      where
--        map' = Map.delete b map
--        seq' = if member b m
--            then hd' Seq.>< (Seq.drop 1 tl')
--            else seq
--        --ind =  -- fromJust $ Seq.elemIndexL b (getOrdSeq m)
--        (hd', tl') = Seq.splitAt (map Map.! b) seq
--        map = getOrdMap m
--        seq = getOrdSeq m
--
--    update f b m = if member b m then update' (f (m!b)) else m
--      where
--        update' (Just v) = insert b v m
--        update' Nothing = delete b m
--
----    map f m = OrdModel $ IOrdModel map' (getOrdSeq m)
----      where
----        map' = Map.map f (getOrdMap m)
----
----    fold f z m = DF.foldl' f' z (getOrdSeq m)
----      where
----        f' z' b = f (m!b) z'
--
--    elems m = DF.toList $ getOrdSeq m
--
--    keys m = List.map fst (toList m)
--
--    toList m = DF.toList . fmap getTopBinding $ getOrdSeq m
--
--    --List.map (\b -> (b, m!b)) (keys m)
--
--    fromList xs = DF.foldl' (\m (b, v) -> insert b v m) empty xs
