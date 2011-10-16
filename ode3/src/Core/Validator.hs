-----------------------------------------------------------------------------
--
-- Module      :  Core.Validator
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | A basic validator for a Module expressions, perfromas very simple validation tasks
--
-----------------------------------------------------------------------------

module Core.Validator (
validate,
) where

import Core.AST as C
import Utils.Utils

validate :: C.Module C.SrcId -> MExcept (C.Module C.SrcId)
validate mod = Right mod

