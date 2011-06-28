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
-- |Renamer takes a reordered AST and renames all variables into unique values, thus it also deals with all scoping
-- issues. As oly are two scopes this should be fairly easy.
-- It also checks for all vlaue declarations, unused values, undefined values, and so on
-- Can issue errors due to user defined model
--
-----------------------------------------------------------------------------

module Core.Renamer (
rename
) where

import qualified Core.AST as C

rename :: C.Model -> Int
rename cModel = 3
