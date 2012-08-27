-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Units.Builtins
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | A collection of built-in units, in most cases we have a units module that defines most units and conversion,
-- however some are built into the language so we can ensure their presence
--
-----------------------------------------------------------------------------

module Subsystem.Units.Builtins (
    createSIs,
    -- builtins
    uSeconds, uMinutes, uHours,
    defQuantities, defUnits, defConvs
) where

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import Data.Monoid

import Subsystem.Units.UnitsDims
import Subsystem.Units.Conversion
import Utils.Utils

createSIs :: UnitDef -> ([UnitDef], [ConvDef])
createSIs (UnitDef baseUnit baseDim) = mapSnd concat . unzip $ siUnitDef
  where
    siUnitDef :: [(UnitDef, [ConvDef])]
    siUnitDef = [
                -- mults
                  mkSIUnit "k" 1e3, mkSIUnit "G" 1e6, mkSIUnit "T" 1e9
                -- fracts
                , mkSIUnit "m" 1e-3, mkSIUnit "u" 1e-6, mkSIUnit "n" 1e-9
                ]

--               -- mults
--                [ mkSIUnit "da", mkSIUnit "h", mkSIUnit "k", mkSIUnit "M", mkSIUnit "G"
--                , mkSIUnit "T", mkSIUnit "P", mkSIUnit "E", mkSIUnit "Z", mkSIUnit "Y"
--                -- fractions
--                , mkSIUnit "d", mkSIUnit "c", mkSIUnit "m", mkSIUnit "u", mkSIUnit "n"
--                , mkSIUnit "p", mkSIUnit "f", mkSIUnit "a", mkSIUnit "z", mkSIUnit "y"
--                ]


    -- mkSIUnit prefix = BaseUnitDef baseDim (prefix ++ baseName) (maybe Nothing (\alias -> Just $ prefix ++ alias))
    mkSIUnit prefix cf = (unitDef, [convF, convR])
      where
        baseUnit' = prefix ++ baseUnit
        -- unit' = mkUnit [(baseUnit', 1)]
        unitDef = UnitDef baseUnit' baseDim

        convF = ConvDef baseUnit baseUnit' (CExpr CDiv CFromId (CNum cf))
        convR = ConvDef baseUnit' baseUnit (CExpr CMul CFromId (CNum cf))

-- initial units state
-- default quantities
defQuantities = addQuantitiesToBimap Bimap.empty builtinQuantities
  where
    builtinQuantities :: Quantities
    builtinQuantities = [("time", dimToDimVec DimT)]

-- base unit defs
-- default units :: BaseUnit
baseSeconds = "s"
baseMinutes = "min"
baseHours = "hr"

uSeconds = mkUnit [(baseSeconds, 1)]
uMinutes = mkUnit [(baseMinutes, 1)]
uHours = mkUnit [(baseHours, 1)]

-- default unit env
(Right defUnits) = addUnitsToEnv Map.empty builtinUnits
  where
    builtinUnits :: [UnitDef]
    builtinUnits =  [ UnitDef baseSeconds DimT
                    , UnitDef baseMinutes DimT
                    , UnitDef baseHours DimT
                    ]

-- builtin unit conversions
(Right defConvs) = addConvsToGraph Map.empty builtinConvs defUnits
  where
    builtinConvs :: [ConvDef]
    builtinConvs =  [ ConvDef baseSeconds baseMinutes (CExpr CDiv CFromId (CNum 60)) -- s -> min = s / 60
                    , ConvDef baseMinutes baseSeconds (CExpr CMul CFromId (CNum 60)) -- inverse
                    , ConvDef baseMinutes baseHours (CExpr CDiv CFromId (CNum 60)) -- min -> hr = min / 60
                    , ConvDef baseHours baseMinutes (CExpr CMul CFromId (CNum 60)) -- inverse
                    ]