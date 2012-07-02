-----------------------------------------------------------------------------
--
-- Module      :  Lang.Core.UnitChecker
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | main algorithms and functionaliy for checking the units of a module
--
-----------------------------------------------------------------------------

module Lang.Core.UnitChecker (
unitCheck

) where

import qualified Lang.Core.AST as E
import qualified Lang.Module.AST as M
import qualified UI.SysState as St

import Lang.Core.Units
import Utils.Utils

-- functions
-- * unit checking
-- * dimenstion changing
-- * unit conversion
-- * module-level auto-conversion

-- TODO - need global modenv?, filedata?
unitCheck :: St.UnitsState -> M.Module E.Id -> MExcept (M.Module E.Id)

unitCheck unitsState mod@(M.LitMod exprMap modData) = do
    -- iterate over the expressions, and checking units along the way


    return mod


unitCheck unitsState mod@(M.FunctorMod args exprMap modData) = do
    return mod


unitCheck' (M.ExprMap E.Id)
