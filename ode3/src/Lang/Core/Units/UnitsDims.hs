-----------------------------------------------------------------------------
--
-- Module      :  Lang.Core.Units.UnitsMain
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Dimensions ( & Quantities) and Units ADTs and functions
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures, DeriveFunctor #-}

module Lang.Core.Units.UnitsDims (
    -- datatypes
    Quantity, Quantities,
    DimVec(..), addDim, subDim, mkDimVec, dimensionless, isZeroDim,
    BaseDim(..), dimToDimVec,
    UnitList, UnitDef(..), Unit(..), BaseUnit, mkUnit, addUnit, subUnit, isBaseUnit,

    -- data structures
    QuantityBimap, UnitDimEnv,

    -- high-level accessor funcs
    addQuantitiesToBimap, addUnitsToEnv, getBaseDim,
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

data Unit   = UnitC UnitList -- an actual unit with a known dimensionless
            | NoUnit                         -- no unit data infered, just a raw number (is this not same as ActualUnit []/dmless ?)
            -- UnknownUnit                    -- We don't know the unit type yet - used with TC
            | UnitVar Int                -- A unit variable, used for unit & dimension polymorphism
                                            -- we can't do much with such types, can operate on the number but always retains it's unit type
            | DMLess        -- an expression that previously had units that were cancelled out
            deriving (Eq, Ord, Show)

--instance Show Unit where
--    show (UnitC units) = List.intercalate "." (map showUnit units)
--      where
--        showUnit (baseUnit, index) = baseUnit ++ "^" ++ show index
--    show NoUnit = "NoUnit"
--    -- show UnknownUnit = "UnknownUnit"
--    show (UnitVar i) = "UnitVar:" ++ (show i)


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


-- Unit helper funcs

-- need to filter dups, process indices, and define ordering
mkUnit :: UnitList -> Unit
mkUnit [] = NoUnit -- we convert an empty list of dimenstinos directly into NoUnit - may be better have a Dmless value instead
mkUnit base@[(baseName, 1) ]= UnitC base
mkUnit units = if length units' == 0 then NoUnit else UnitC units'
  where
    -- sum all the base units, filter canceled units
    units' = Map.toList . Map.filter (\idx -> not (idx == 0)) . foldl collectBUs Map.empty $ units
    -- collect the baseunits
    collectBUs unitMap (unitName, index) = Map.insertWith' (+) unitName index unitMap


isBaseUnit :: Unit -> Bool
isBaseUnit (UnitC [(baseName, 1)]) = True
isBaseUnit _ = False

-- do we need any unitsstate for this, i.e. unitdimenv, do we need the dimensions?
addUnit :: Unit -> Unit -> Unit
addUnit (UnitC u1') (UnitC u2') = trace' [MkSB u1', MkSB u2'] "addUnits" $ mkUnit $ u1' ++ u2'
addUnit NoUnit u2 = u2
addUnit u1 NoUnit = u1

-- just negate the second unit
subUnit :: Unit -> Unit -> Unit
subUnit u1 u2 = addUnit u1 $ negUnit u2

negUnit :: Unit -> Unit
negUnit (UnitC u') = UnitC $ map (mapSnd negate) u'
negUnit NoUnit = NoUnit

-- Unit Env helper funcs
-- calculate on-demand the dimension of a derived unit
calcUnitDim :: Unit -> UnitDimEnv -> MExcept DimVec
-- handle case of un-dimensioned values explitictly here
calcUnitDim NoUnit uEnv = throwError $ printf "Cannot obtain a dimension for a NoUnit"
calcUnitDim u@(UnitC units) uEnv = mconcat <$> mapM getDim units
  where
    getDim (baseUnit, index) = mulDim <$> (dimToDimVec <$> lookupBUnitDim baseUnit uEnv) <*> pure index

lookupBUnitDim :: BaseUnit -> UnitDimEnv -> MExcept BaseDim
lookupBUnitDim u uEnv =
    maybeToExcept (Map.lookup u uEnv) $ printf "Reference to unknown base unit \'%s\' found" u

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
