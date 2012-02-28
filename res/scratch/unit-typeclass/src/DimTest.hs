-----------------------------------------------------------------------------
--
-- Module      :  DimTest
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Test the dimensional pacakge
--
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module DimTest where


import qualified Prelude
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (foot, yard)

-- create a simple sum using metres
length1 = 2 *~ meter
length2 = 3 *~ foot
length3 = 20 *~ milli meter
length4 = 1 *~ yard

-- auto converts to base
dimTest1 = length1 + length2
dimTest2 = length3 + length2
dimTest3 = length2 + length4

-- try casting - doesn't work, always in metres
dimTest4 :: Length Double  = (length1 + length2)

-- adding dimensionless - doesn't work, can multiply tho
-- dimTest5 = length1 + length2 + _3
dimTest5 = length1 + length2 * _3

-- multiplying dimensions - returns m^2
dimTest6 = length1 * length2

-- multiplying invalid/nonsense dimensions
dimTest7 = length1 * (2 *~ second)
-- dividing and dimensionless arith
dimTest8 = (length1 / length2) + (2 *~ one)

-- add values of same base unit - allowed, uses s^-1
dimTest9 = (1 *~ hertz) + (2 *~ becquerel)
