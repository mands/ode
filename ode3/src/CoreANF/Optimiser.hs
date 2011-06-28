-----------------------------------------------------------------------------
--
-- Module      :  CoreANF.Optimiser
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |The optimisation stage, basically a driver for all the optimisers that will be created to modiy and transform the AST
-- many optimisations will be present, including
-- constant folding/propogation, CSE, function inlining (across module boundaries), vectorisation
-----------------------------------------------------------------------------

module CoreANF.Optimiser (
optimise
) where

import qualified CoreANF.AST as CA

optimise :: CA.Model -> CA.Model
optimise a = a
