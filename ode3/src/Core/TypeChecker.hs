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
--
-----------------------------------------------------------------------------

module Core.TypeChecker (
typeCheck
) where

import qualified Core.AST as C

reorder :: C.Model -> Int
reorder cModel = 3
