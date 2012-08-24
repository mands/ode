-----------------------------------------------------------------------------
--
-- Module      :  Lang.Core.Flatten
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | TODO - main functioalyity that takes a set of Core modules and flattens them into CoreFlat AST
-- involves
-- * inline all modules
-- * inline components
-- * convert all units & drop unneeded type-checking info (wraps/newtypes and units)
-- * convert to ANF and CoreFlat AST

-- Notes
-- * use intmap to hold local ids
-- * need create a new ADT to hold metadata
-----------------------------------------------------------------------------

module Process.Flatten (
flatten
) where

import Utils.CommonImports
import Subsystem.SysState

flatten :: String -> SysExcept ()
flatten initMod = return ()
