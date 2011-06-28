-----------------------------------------------------------------------------
--
-- Module      :  Core.Desugarer
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |Desugarer - takes an Ode AST and desguars and converts into the Core langauge AST
-- Can issue errors due to user defined model
--
-----------------------------------------------------------------------------

module Core.Desugarer (
desugar
) where

import qualified Ode.AST as O
import qualified Core.AST as C

desugar :: O.Model -> C.Model
desugar oModel = [4]
