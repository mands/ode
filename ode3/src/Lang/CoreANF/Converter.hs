-----------------------------------------------------------------------------
--
-- Module      :  CoreANF.Converter
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |Converter module takes a final, type-checked Core AST and performs coversion into ANF ready for the middle-end of
-- the compiler - where all optimisations will take place
-- No user-based errors should occur at this stage
--
-----------------------------------------------------------------------------

module CoreANF.Converter (
convert
) where

import qualified Core.AST as C
import qualified CoreANF.AST as CA

convert :: C.Model -> CA.Model
convert cModel = [1]


