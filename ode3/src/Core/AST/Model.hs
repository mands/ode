-----------------------------------------------------------------------------
--
-- Module      :  Core.AST.Model
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Not yet sure what a model means - is this actually used/needed or is it to become part of the module system
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies  #-}
{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

module Core.AST.Model (
    ModelMap(..), Model, ListModel, OrdModel, getOrdMap, getOrdSeq, putOrdMap, putOrdSeq
) where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import qualified Data.List as List
import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import qualified Data.Functor as Functor
import Data.Maybe (fromJust, isJust)
import Utils.Utils
import Core.AST.Expr


-- TODO -- maybe use number and Id to index/key
-- | Top level Core model
-- need to make sure this is an ordered map so we keep the evaluation order correct
type Model b =  Map.Map (Bind b) (Top b)

type ListModel b = [(Bind b, Top b)]

--data OrdModel b = OrdModel { ordMap :: IntMap.IntMap b, ordSeq :: Seq.Seq b }
data IOrdModel b = IOrdModel { ordMap :: Map.Map (Bind b) Int, ordSeq :: Seq.Seq (Top b) } deriving Show

-- Newtype test - is it needed in this case - type classes should be enough with escape hatch if neccessary
newtype OrdModel b = OrdModel { getOrdModel :: IOrdModel b } deriving Show
getOrdMap :: OrdModel b -> Map.Map (Bind b) Int
getOrdMap om = ordMap . getOrdModel $ om

putOrdMap :: Map.Map (Bind b) Int -> OrdModel b -> OrdModel b
putOrdMap map m = OrdModel $ IOrdModel map (getOrdSeq m)

getOrdSeq :: OrdModel b -> Seq.Seq (Top b)
getOrdSeq om = ordSeq . getOrdModel $ om

putOrdSeq :: Seq.Seq (Top b) -> OrdModel b -> OrdModel b
putOrdSeq seq m = OrdModel $ IOrdModel (getOrdMap m) seq


-- |standard typeclass for a top-level model map structure
-- most functions cribbed from Data.Map
-- add more as needed, only basic essential funcs here, rest acessible via escape hatch to direct map
-- need to make instance of Traversable and Foldable
class ModelMap a b | a -> b where -- a is the Data Strcuture, b is the Bind Type Variable
    -- | Perform a direct lookup for given binding, throw error if not found
    (!) :: (Ord b) => a -> (Bind b) -> Top b
    -- | return the topval within a Maybe for the given binding
    lookup :: (Ord b) => (Bind b) -> a -> Maybe (Top b) -- keep same order args as Data.Map (even tho wrong)
    -- | Check if the binding is within the model
    member :: (Ord b) => (Bind b) -> a -> Bool
    -- | Create an empty model
    empty :: a
    -- | Create a new model from a single top-binding
    singleton :: (Bind b) -> (Top b) -> a
    -- | Insert the binding into the model, taking into account the insertion order
    insert :: (Ord b) => (Bind b) -> (Top b) -> a -> a
    -- | Delete the binding from the model
    delete :: (Ord b) => (Bind b) -> a -> a
    -- | if elem exists, run the function over the topval, if returns Just then update, if None then delete
    update :: (Ord b) => (Top b -> Maybe (Top b)) -> (Bind b) -> a -> a
    -- | Map over elements in order independent manner,
    -- does not allow for chaning the types of the binding
    -- should this then run fmap over the internal elems?
    -- provided by instances of Functor and Traversable
    --map :: (Top b -> Top b) -> a -> a
    -- | Fold over the elements, taking into account the ordering
    --fold :: (Ord b) => (Top b -> c -> c) -> c -> a -> c
    -- | The top-level elements in the model
    elems :: (Ord b) => a -> [Top b]
    -- | The top-level bindings in the model
    keys  :: (Ord b) => a -> [Bind b]
    -- | An association-list repsentation of the model
    toList :: (Ord b) => a -> [(Bind b, Top b)] -- this may be ordered depending on the type of model
    -- | Convert an assoication list binding/top-elem into a model
    fromList :: Ord b => [(Bind b, Top b)] -> a -- this initial ordering may be retained


-- create a few typeclasss instances

-- | Basic wrapper around Data.Map
instance ModelMap (Model b) b where
    (!) m b = m Map.! b
    lookup b m = Map.lookup b m
    member b m = Map.member b m
    empty = Map.empty
    singleton b v = Map.singleton b v
    insert b v m = Map.insert b v m
    delete b m = Map.delete b m
    update f b m = Map.update f b m
    --map f m = Map.map f m -- Map.map (\v -> fmap f v) m
    --fold f z m = Map.fold f z m
    elems m = Map.elems m
    keys m = Map.keys m
    toList m = Map.toList m
    fromList xs = Map.fromList xs

-- | this model rep holds insertion order correctly but is inefficent (O(N))on lookups
-- the order is stored from left-to-right within the list, with ealier ordered elems coming first within the list
instance ModelMap (ListModel b) b where
    (!) m b = fromJust $ List.lookup b m
    lookup b m = List.lookup b m
    member b m = isJust $ List.lookup b m
    empty = []
    singleton b v = (b,v) : []

    -- if already exsists use existing insertion order, else add at the tail of list
    insert b v m = maybe insert' update' mInd
      where
        update' i = let (hd', tl') = splitAt i m
                    in hd' ++ ((b, v) : (tail tl'))
        insert' = m ++ [(b,v)]
        mInd = List.elemIndex b (keys m)

    delete b m = maybe m delete' mInd
      where
        mInd = List.elemIndex b (keys m)
        delete' i = let (hd', tl') = splitAt i m
                    in hd' ++ (tail tl')

    update f b m = if member b m then update' (f (m!b)) else m
      where
        update' (Just v) = insert b v m
        update' Nothing = delete b m

    --map f m = List.map (\(b, v) -> (b, f v)) m
    --fold f z m = List.foldl (\z (b, v) -> f v z) z m -- do a left fold as in order
    elems m = List.map snd m
    keys m = List.map fst m
    toList = id
    fromList = id

-- | Optimised OrdModel that uses both a map and a seq to hold the ordering of elements
instance ModelMap (OrdModel b) b where
    (!) m b = Seq.index (getOrdSeq m) ((getOrdMap m) Map.! b)
    lookup b m = Map.lookup b (getOrdMap m) >>= (\i -> return $ Seq.index (getOrdSeq m) i)
    member b m = Map.member b (getOrdMap m)
    empty = OrdModel $ IOrdModel Map.empty Seq.empty
    singleton b v = OrdModel $ IOrdModel (Map.singleton b 0) (Seq.singleton v)
    insert b v m = OrdModel $ IOrdModel map' seq'
      where
        map' = if member b m then map else Map.insert b (Seq.length seq) map
        seq' = if member b m -- if already in model
            then Seq.update (map Map.! b) v seq  -- then update in cur pos
            else seq Seq.|> v -- else append to end of seq
        map = getOrdMap m
        seq = getOrdSeq m

    delete b m = OrdModel $ IOrdModel map' seq'
      where
        map' = Map.delete b map
        seq' = if member b m
            then hd' Seq.>< (Seq.drop 1 tl')
            else seq
        --ind =  -- fromJust $ Seq.elemIndexL b (getOrdSeq m)
        (hd', tl') = Seq.splitAt (map Map.! b) seq
        map = getOrdMap m
        seq = getOrdSeq m

    update f b m = if member b m then update' (f (m!b)) else m
      where
        update' (Just v) = insert b v m
        update' Nothing = delete b m

--    map f m = OrdModel $ IOrdModel map' (getOrdSeq m)
--      where
--        map' = Map.map f (getOrdMap m)
--
--    fold f z m = DF.foldl' f' z (getOrdSeq m)
--      where
--        f' z' b = f (m!b) z'

    elems m = DF.toList $ getOrdSeq m

    keys m = List.map fst (toList m)

    toList m = DF.toList . fmap getTopBinding $ getOrdSeq m

    --List.map (\b -> (b, m!b)) (keys m)

    fromList xs = DF.foldl' (\m (b, v) -> insert b v m) empty xs

-- |Basic instances

-- this can't be done as the contained object within OrdModel is not the same as that within the sequence (i.e. b vs Top b)
-- instead need to get the seq directly to fold over
--instance DF.Foldable OrdModel where
    --foldr :: (a -> b -> b) -> b -> t a -> b
--    foldr f z m = DF.foldr f z (getOrdSeq m)

-- Functor instance - again can't do for same reason as above, model holds b, seq holds (Top b) - not compatible
--instance Functor OrdModel where
    -- need to fmap over the sequence, then recreate the orig structure using insert with a fold
--    fmap f model = model
--      where
--        newSeq = fmap f (getOrdSeq model)



instance (Show a) => PrettyPrint (Model a) where
    prettyPrint model = unlines . Prelude.map (\e -> prettyPrint e) $ Map.elems model


