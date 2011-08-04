-----------------------------------------------------------------------------
--
-- Module      :  Core.TypeChecker
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |Type checker takes a renamed AST and perfroms type-inference, this is based upon the HM-algorithm, but without
-- let-polymorphism, recursive defintions and so on.
-- Very few primitive types are allowed and user defiend compound types disallowed - hence should be simple
-- Allowed types are, Numerical (Float/Int), Boolean, Function, n-Pair, Ode, and Reaction
-- Can issue errors due to user defined model
-- TODO
-- Use GADTs to enforce terms of correct types (can we do this in earlier expr AST?)
-----------------------------------------------------------------------------

module Core.TypeChecker (
typeCheck
) where

import qualified Data.Map as Map
import qualified Core.AST as C


typeCheck :: C.OrdModel Int -> C.OrdModel Int
typeCheck cModel = C.empty

-- Easy-HM -
-- run a pass over the model, collecting all elems into a multi-map of possible types
-- then map over the multi-map, for each eleemnt unifiying the possible types and making sure they match
-- eventually have a single type for each elem, add to the binding sites and all is good
newtype TypeMap = Map.Map Int C.TType
