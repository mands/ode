-----------------------------------------------------------------------------
--
-- Module      :  Core.Reorderer
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |Reorderer takes a Core AST and performs a basic reordering of the stamenets
-- all values are defiend before usage
-- At the same time the reorderer tests for cyclic dependenceis between value
-- definitions and in function calls
-- Can issue errors due to user defined model
--
-----------------------------------------------------------------------------

module Core.Reorderer (
reorder
) where

import qualified Core.AST as C

reorder :: C.Model -> Int
reorder cModel = 3
