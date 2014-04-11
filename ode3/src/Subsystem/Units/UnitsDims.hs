-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Units.UnitsDims
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Dimensions ( & Quantities) and Units ADTs and functions
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures, DeriveFunctor, PatternGuards #-}

module Subsystem.Units.UnitsDims (
    -- datatypes
    Quantity, Quantities,
    DimVec(..), addDim, subDim, mulUnit, divUnit, mkDimVec, dimensionless, isZeroDim,
    BaseDim(..), dimToDimVec,
    UnitList, UnitDef(..), Unit(..), BaseUnit, mkUnit, addUnit, subUnit, isBaseUnit,
    DerivedUnits,

    -- data structures
    QuantityBimap, UnitDimEnv, DerivedUnitEnv,

    -- high-level accessor funcs
    addQuantitiesToBimap, addUnitsToEnv, addDerivedUnitsToEnv, getBaseDim, expandDerivedUnit,
    calcUnitDim, getDimForUnits, unitsSameDim,
    getBDimForBUnits, lookupBUnitDim
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.State (runState)
import Text.Printf (printf)

import qualified Data.Foldable as DF
import qualified Data.List as List
import qualified Data.List.Split as ListSplit
import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import Data.Monoid

import Utils.CommonImports

-- Dimensions ----------------------------------------------------------------------------------------------------------

-- Dimension Vector
-- Dim L M T I O J N
data DimVec = DimVec    { dimL :: Integer, dimM :: Integer, dimT :: Integer, dimI :: Integer
                        , dimO :: Integer, dimJ :: Integer, dimN :: Integer
                        } deriving (Show, Eq, Ord)

mkDimVec = DimVec 0 0 0 0 0 0 0
dimensionless = DimVec 0 0 0 0 0 0 0

data BaseDim = DimL | DimM | DimT | DimI | DimO | DimJ | DimN deriving (Show, Eq, Ord)

dimToDimVec :: BaseDim -> DimVec
dimToDimVec DimL = DimVec 1 0 0 0 0 0 0
dimToDimVec DimM = DimVec 0 1 0 0 0 0 0
dimToDimVec DimT = DimVec 0 0 1 0 0 0 0
dimToDimVec DimI = DimVec 0 0 0 1 0 0 0
dimToDimVec DimO = DimVec 0 0 0 0 1 0 0
dimToDimVec DimJ = DimVec 0 0 0 0 0 1 0
dimToDimVec DimN = DimVec 0 0 0 0 0 0 1

dimVecToDim :: DimVec -> BaseDim
dimVecToDim (DimVec 1 0 0 0 0 0 0) = DimL
dimVecToDim (DimVec 0 1 0 0 0 0 0) = DimM
dimVecToDim (DimVec 0 0 1 0 0 0 0) = DimT
dimVecToDim (DimVec 0 0 0 1 0 0 0) = DimI
dimVecToDim (DimVec 0 0 0 0 1 0 0) = DimO
dimVecToDim (DimVec 0 0 0 0 0 1 0) = DimJ
dimVecToDim (DimVec 0 0 0 0 0 0 1) = DimN
dimVecToDim dim = errorDump [MkSB dim] "Passed an invalid dimVec to get a baseDim" assert


getBaseDim :: Char -> BaseDim
getBaseDim 'L' = DimL
getBaseDim 'M' = DimM
getBaseDim 'T' = DimT
getBaseDim 'I' = DimI
getBaseDim 'O' = DimO
getBaseDim 'J' = DimJ
getBaseDim 'N' = DimN
getBaseDim c = errorDump [MkSB c] "Parsed an invalid dimension" assert

-- Dim helper funcs
addDim (DimVec l m t i o j n) (DimVec l' m' t' i' o' j' n') =
    DimVec (l+l') (m+m') (t+t') (i+i') (o+o') (j+j') (n+n')

subDim (DimVec l m t i o j n) (DimVec l' m' t' i' o' j' n') =
    DimVec (l-l') (m-m') (t-t') (i-i') (o-o') (j-j') (n-n')

mulDim (DimVec l m t i o j n) x = DimVec (l*x) (m*x) (t*x) (i*x) (o*x) (j*x) (n*x)

-- special handling here
divDim :: DimVec -> Integer -> MExcept DimVec
divDim dim x | x < 1 = throwError $ printf "Dimension error - can't divide %s by zero or negative integer" (show dim)
divDim dim@(DimVec l m t i o j n) x = DimVec <$> (divDim' l) <*> (divDim' m) <*> (divDim' t) <*> (divDim' i) <*> (divDim' o) <*> (divDim' j) <*> (divDim' n)
  where
    divDim' d | (quot, 0) <- quotRem d x = return quot
    divDim' d | (_, rem) <- quotRem d x = throwError $ printf "Dimension error - can't divide %s by %s, would create non-integer dimension" (show dim) (show x)

isZeroDim = (==) dimensionless

negDim (DimVec l m t i o j n) = DimVec (negate l) (negate m) (negate t) (negate i) (negate o) (negate j) (negate n)

instance Monoid DimVec where
    mempty = dimensionless
    mappend = addDim


-- Quantities ----------------------------------------------------------------------------------------------------------
type Quantity = String
type QuantityBimap = Bimap.Bimap Quantity DimVec
type Quantities = [(Quantity, DimVec)]

-- Quantity helper funcs
addQuantitiesToBimap :: QuantityBimap -> Quantities -> QuantityBimap
addQuantitiesToBimap = foldl (\qBimap (quantity, dimVec) -> Bimap.insert quantity dimVec qBimap)

-- Units ---------------------------------------------------------------------------------------------------------------
-- newtype Unit = UnitC [(String, Integer)] deriving (Eq, Ord)

--instance Show Unit where
--    show (UnitC units) = List.intercalate "." (map showUnit units)
--      where
--        showUnit (baseName, index) = baseName ++ show index

type BaseUnit = String


-- NOTE - currently we do not distinguish between NoUnit and DMLess - a unitless number and an expressions
-- that cancels units are equal in terms of the system. Canc hange this later if required, to allow
-- casting DMLess exprs.
-- Also, we do not consider NoUnit (and thus DMLess) to have a dimension at present, instead throw an error
data Unit   = UnitC UnitList -- an actual unit with a known dimensionless
            | NoUnit                         -- no unit data infered, just a raw number (is this not same as ActualUnit []/dmless ?)
            -- UnknownUnit                    -- We don't know the unit type yet - used with TC
            | UnitVar Integer                -- A unit variable, used for unit & dimension polymorphism
                                            -- we can't do much with such types, can operate on the number but always retains it's unit type
            | DMLess        -- an expression that previously had units that were cancelled out
            | DerivedUnit UnitList
            deriving (Eq, Ord, Show)

type UnitList = [(BaseUnit, Integer)]

-- hold this temp structure in indiv module, and promote to global state (UnitDimEnv) when imported & processed
data UnitDef = UnitDef BaseUnit BaseDim
    -- DerivedUnitDef Unit
    deriving (Eq, Ord, Show)

-- type BaseUnitMap = Map.Map Unit Integer
--type UnitAlias = String
--type UnitAliasBimap = Bimap.Bimap UnitAlias Unit

-- TODO - can these structures be simplified/unified
-- mapping from (base?) units to dimensions
type UnitDimEnv = Map.Map BaseUnit BaseDim

type DerivedUnitEnv = Map.Map String UnitList
type DerivedUnits = [(String, UnitList)]

-- Quantity helper funcs
addDerivedUnitsToEnv :: DerivedUnitEnv -> DerivedUnits -> DerivedUnitEnv
addDerivedUnitsToEnv = foldl (\dEnv (dName, dUnit) -> Map.insert dName dUnit dEnv)

-- Unit helper funcs ---------------------------------------------------------------------------------------------------------------

expandDerivedUnit :: Unit -> Unit
expandDerivedUnit (DerivedUnit ul) = mkUnit ul

-- need to filter dups, process indices, and define ordering
-- we consider an (passed or dervied) empty list as a NoUnit - may be better have a Dmless value instead
mkUnit :: UnitList -> Unit
mkUnit [] = NoUnit
mkUnit base@[(baseName, 1) ]= UnitC base
mkUnit units = if length units' == 0 then NoUnit else UnitC units'
  where
    -- sum all the base units, filter canceled units
    units' = Map.toList . Map.filter (notEqual 0) . foldl collectBUs Map.empty $ units
    -- collect the baseunits
    collectBUs unitMap (unitName, index) = Map.insertWith' (+) unitName index unitMap


isBaseUnit :: Unit -> Bool
isBaseUnit (UnitC [(baseName, 1)]) = True
isBaseUnit _ = False

-- do we need any unitsstate for this, i.e. unitdimenv, do we need the dimensions?
addUnit :: Unit -> Unit -> Unit
addUnit (UnitC u1') (UnitC u2') = mkUnit $ u1' ++ u2'
addUnit NoUnit u2 = u2
addUnit u1 NoUnit = u1
addUnit u1 u2 = errorDump [MkSB u1, MkSB u2] "addunit" assert

-- just negate the second unit
subUnit :: Unit -> Unit -> Unit
subUnit u1 u2 = addUnit u1 $ negUnit u2

negUnit :: Unit -> Unit
negUnit (UnitC u') = UnitC $ map (mapSnd negate) u'
negUnit NoUnit = NoUnit

-- we could just recurse and add the unit x times
mulUnit :: Unit -> Integer -> Unit
mulUnit NoUnit _ = NoUnit
mulUnit (UnitC u1') x = mkUnit $ map (\(u, i) -> (u, i*x)) u1'

divUnit :: Unit -> Integer -> MExcept Unit
divUnit NoUnit _ = return NoUnit
divUnit u1 x | x < 1  = throwError $ printf "Unit Error - can't divide %s by zero or negative integer" (show u1)
divUnit u1@(UnitC u1') x = UnitC <$> mapM divBUnit u1'
  where
    divBUnit (u, i) | (quot, 0) <- quotRem i x = return (u, quot)
    divBUnit (u, i) | (_, rem)  <- quotRem i x = throwError $ printf "Unit Error - can't divide %s by %s, would create non-integer dimension" (show u1) (show x)

-- Unit Env helper funcs
-- calculate on-demand the dimension of a derived unit
calcUnitDim :: Unit -> UnitDimEnv -> MExcept DimVec
-- handle case of un-dimensioned values explitictly here
-- calcUnitDim NoUnit uEnv = throwError $ printf "NoUnit values do not have a dimension"
calcUnitDim NoUnit uEnv = return dimensionless

calcUnitDim u@(UnitC units) uEnv = mconcat <$> mapM getDim units
  where
    getDim (baseUnit, index) = mulDim <$> (dimToDimVec <$> lookupBUnitDim baseUnit uEnv) <*> pure index

lookupBUnitDim :: BaseUnit -> UnitDimEnv -> MExcept BaseDim
lookupBUnitDim u uEnv = u' -- trace' [MkSB u, MkSB uEnv] "unit lookup" $ u'
  where
    u' = maybeToExcept (Map.lookup u uEnv) $ printf "Reference to unknown base unit \'%s\' found" u

-- Add a list of units to the UnitEnv
-- if a baseUnit, add it directly with the associated dimension-- if a dervied unit, ignore
addUnitsToEnv :: UnitDimEnv -> [UnitDef] -> MExcept UnitDimEnv
addUnitsToEnv unitEnv units = DF.foldlM addUnit unitEnv units
  where
    addUnit unitEnv (UnitDef u d) = case Map.lookup u unitEnv of
        Nothing -> return $ Map.insert u d unitEnv
        Just _ -> throwError $ printf "Base unit \'%s\' already defined" (show u)

-- | get the dimensions for both units (if they exist), then check they are same and return the dim
getDimForUnits :: Unit -> Unit -> UnitDimEnv -> MExcept DimVec
getDimForUnits u1 u2 uEnv = do
    dim1 <- calcUnitDim u1 uEnv
    dim2 <- calcUnitDim u2 uEnv
    if dim1 /= dim2 then
        throwError $ printf "Dimension mismatch - units %s (%s) and %s (%s)" (show u1) (show dim1) (show u2) (show dim2)
        else return dim1

-- | simple wrapper around getDimForUnits with short-circuit for equal units
unitsSameDim :: Unit -> Unit -> UnitDimEnv -> MExcept ()
unitsSameDim u1@(UnitC _) u2@(UnitC _) uEnv | u1 == u2 = return ()
unitsSameDim u1 u2 uEnv = getDimForUnits u1 u2 uEnv >> return ()

-- | get the dimensions for both baseunits (if they exist), then check they are same and return the dim
getBDimForBUnits :: BaseUnit -> BaseUnit -> UnitDimEnv -> MExcept BaseDim
getBDimForBUnits u1 u2 uEnv = do
    dim1 <- lookupBUnitDim u1 uEnv
    dim2 <- lookupBUnitDim u2 uEnv
    if dim1 /= dim2 then
        throwError $ printf "Dimension mismatch - baseunits %s (%s) and %s (%s)" (show u1) (show dim1) (show u2) (show dim2)
        else return dim1
