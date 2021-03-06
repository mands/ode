-----------------------------------------------------------------------------
--
-- Module      :  Utils.OrdSet
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | A little wrapper around List to support set functions that also retain their insertion order
--
-----------------------------------------------------------------------------

module Ode.Utils.OrdSet where -- export all

import qualified Data.List as List

-- a light wrapper aroudn list so we semantically can differentate from normal lists
newtype OrdSet a = OrdSetC [a] deriving (Show, Eq)

length (OrdSetC s) = List.length s

empty = OrdSetC []

null (OrdSetC s) = List.null s

-- check elem isn't already there, if not then insert at back of list
insertB v (OrdSetC s) =
    case (List.elem v s) of
    True -> OrdSetC s
    False -> OrdSetC $ s ++ [v]

insertF v (OrdSetC s) =
    case (List.elem v s) of
    True -> OrdSetC s
    False -> OrdSetC $ v : s

delete v (OrdSetC s) = OrdSetC $ List.delete v s

elem v (OrdSetC s) = List.elem v s

(\\) (OrdSetC s1) (OrdSetC s2) = OrdSetC $ (List.\\) s1 s2

union (OrdSetC s1) (OrdSetC s2) = OrdSetC $ List.union s1 s2

intersect (OrdSetC s1) (OrdSetC s2) = OrdSetC $ List.intersect s1 s2

toList (OrdSetC s) = s

-- need to remove dups first
fromList s = OrdSetC $ List.nub s
