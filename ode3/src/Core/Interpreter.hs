-----------------------------------------------------------------------------
--
-- Module      :  Core.Interpreter
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Core.Interpreter (
interpret
) where

import qualified Core.AST as CA

interpret :: CA.Model -> Int
interpret cAST = 1
