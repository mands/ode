-----------------------------------------------------------------------------
--
-- Module      :  CoreANF.Interpreter
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |A reference interpreter for the CoreANF format that is used to check for correctness of each optimisation pass
-- Are other validators required?
--
-----------------------------------------------------------------------------

module CoreANF.Interpreter (

) where

import qualified CoreANF.AST as CA

interpret :: CA.Model -> Int
interpret caModel = 2
