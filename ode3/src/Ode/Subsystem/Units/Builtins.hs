-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Units.Builtins
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | A collection of built-in units, in most cases we have a units module that defines most units and conversion,
-- however some are built into the language so we can ensure their presence
--
-----------------------------------------------------------------------------

module Ode.Subsystem.Units.Builtins (
    createSIs,
    -- builtins
    uSeconds, uMinutes, uHours, uMilliSeconds, uMicroSeconds, getTimeUnit,
    defQuantities, defUnits, defConvs
) where

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import Data.Monoid

import Ode.Subsystem.Units.UnitsDims
import Ode.Subsystem.Units.Conversion
import Ode.Utils.Utils

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
-- this includes all basic time units, as these are required to have a unit-correct time parameter
baseMicroSeconds    = "us"
baseMilliSeconds    = "ms"
baseSeconds         = "s"
baseMinutes         = "min"
baseHours           = "hr"

uMicroSeconds    = mkUnit [(baseMicroSeconds, 1)]
uMilliSeconds   = mkUnit [(baseMilliSeconds, 1)]
uSeconds        = mkUnit [(baseSeconds, 1)]
uMinutes        = mkUnit [(baseMinutes, 1)]
uHours          = mkUnit [(baseHours, 1)]

-- hardcode parser for built in time units
getTimeUnit :: String -> Maybe Unit
getTimeUnit "us"    = Just uMicroSeconds
getTimeUnit "ms"    = Just uMilliSeconds
getTimeUnit "s"     = Just uSeconds
getTimeUnit "min"   = Just uMinutes
getTimeUnit "hr"    = Just uHours
getTimeUnit _       = Nothing

-- default unit env
(Right defUnits) = addUnitsToEnv Map.empty builtinUnits
  where
    builtinUnits :: [UnitDef]
    builtinUnits =  [ UnitDef baseMicroSeconds DimT
                    , UnitDef baseMilliSeconds DimT
                    , UnitDef baseSeconds DimT
                    , UnitDef baseMinutes DimT
                    , UnitDef baseHours DimT
                    ]

-- builtin unit conversions
(Right defConvs) = addConvsToGraph Map.empty builtinConvs defUnits
  where
    builtinConvs :: [ConvDef]
    builtinConvs =  [ ConvDef baseMicroSeconds baseMilliSeconds (CExpr CDiv CFromId (CNum 1000)) -- us -> ms = s / 1000
                    , ConvDef baseMilliSeconds baseMicroSeconds (CExpr CMul CFromId (CNum 1000)) -- inverse

                    , ConvDef baseMilliSeconds baseSeconds (CExpr CDiv CFromId (CNum 1000)) -- ms -> s = s / 1000
                    , ConvDef baseSeconds baseMilliSeconds (CExpr CMul CFromId (CNum 1000)) -- inverse

                    , ConvDef baseSeconds baseMinutes (CExpr CDiv CFromId (CNum 60)) -- s -> min = s / 60
                    , ConvDef baseMinutes baseSeconds (CExpr CMul CFromId (CNum 60)) -- inverse

                    , ConvDef baseMinutes baseHours (CExpr CDiv CFromId (CNum 60)) -- min -> hr = min / 60
                    , ConvDef baseHours baseMinutes (CExpr CMul CFromId (CNum 60)) -- inverse
                    ]
