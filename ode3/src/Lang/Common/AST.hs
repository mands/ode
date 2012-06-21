-----------------------------------------------------------------------------
--
-- Module      :  Common.AST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | common AST Types, ADTs, etc.
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures, DataKinds #-}

module Lang.Common.AST (
    NumTy, UntypedId, Id,
    SrcId, UnitT, DesId,

    ModImport(..), ModURIElems, ModRoot, ModName(..), ModFullName(..),
    mkModRoot, getModRootStr, mkModFullName,splitModFullName,

    Quantity, Quantities, QuantityBimap,
    DimVec(..), UnitDef(..), addDim, subDim, mkDimVec, dimensionless, isZeroDim,
    Unit, mkUnit, SrcUnit, CExpr(..), COp(..), ConvDef(..)
) where


import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Text.Printf (printf)

import qualified Data.Foldable as DF
import qualified Data.List as List
import qualified Data.List.Split as ListSplit
import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import Data.Monoid

-- import Lang.Core.Units
import Utils.Utils

-- Shared Identifiers --------------------------------------------------------------------------------------------------

-- | an individual number type, not sure if needed, used to convert from double to integer
type NumTy = Double

type UnitT = String

-- TODO - change to newtype?
-- | Identifier - basicially RdrName - needs to become parameterised, hold the id and the optional unit annotation
type SrcId = String
type UntypedId = Int
type Id = UntypedId
type DesId = (SrcId, Maybe UnitT)

-- Shared Module AST ---------------------------------------------------------------------------------------------------

-- | import cmds, has a module root/filename, and list of indiv modules and potential alias
data ModImport = ModImport ModRoot (Maybe [(ModName, Maybe ModName)]) deriving (Show, Eq, Ord)

-- TODO - update to use DataKinds and GADTs for type-safe expressions and modules
-- | a canoical module name
-- type ModURI = String

-- ModURI - a list of string indeitifiers that may be a root or mod full name
type ModURIElems = [String]

-- TODO - consider switching rep
-- ModRoot = ModRoot (Maybe String)
-- ModFullName = ModFullName (Maybe String) String

-- ModRoot - a dot-notation root name
newtype ModRoot = ModRoot String deriving (Show, Eq, Ord)
newtype ModName = ModName String deriving (Show, Eq, Ord)
-- newtype ModFullName = ModFullName String deriving (Show, Eq, Ord)

-- implies a reolved module name that may either be fully-revoled, or a local ref to a file/mod env
data ModFullName   = ModFullName ModRoot ModName
                   | ModLocalName ModName
                   deriving (Show, Eq, Ord)

-- data ModImport = ModImport String (Maybe String) deriving (Show, Eq, Ord)

-- smart constructors for ModRoot and ModFullName
-- | Flattens a list of URI elems into dot-notation
mkModRoot :: ModURIElems -> ModRoot
mkModRoot = ModRoot . List.intercalate "."

getModRootStr :: ModRoot -> String
getModRootStr (ModRoot rootStr) = rootStr

-- | takes a list of URI elems and modulename into a dot-notation
mkModFullName :: ModRoot -> ModName -> ModFullName
mkModFullName = ModFullName
--
--mkModFullName (Just (ModRoot modURI)) (ModName shortName) = ModFullName $ modURI ++ "." ++ shortName
--mkModFullName Nothing (ModName shortName) = ModFullName $ shortName

splitModFullName :: ModFullName -> (ModRoot, ModName)
splitModFullName (ModFullName root name) = (root, name)

--splitModFullName (ModFullName fullName) = if length elems == 1 then (Nothing, modName) else (Just modRoot, modName)
--  where
--    elems = ListSplit.splitOn "." fullName
--    modRoot = mkModRoot $ List.init elems
--    modName = ModName $ List.last elems


-- Units, Dimensions and Quantities ------------------------------------------------------------------------------------

-- Dimension Vector
-- Dim L M T I O J N
data DimVec = DimVec    { dimL :: Integer, dimM :: Integer, dimT :: Integer, dimI :: Integer
                        , dimO :: Integer, dimJ :: Integer, dimN :: Integer
                        } deriving (Show, Eq, Ord)

mkDimVec = DimVec 0 0 0 0 0 0 0
dimensionless = DimVec 0 0 0 0 0 0 0

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

-- Quantities
type Quantity = String
type QuantityBimap = Bimap.Bimap Quantity DimVec
type Quantities = [(Quantity, DimVec)]

-- Quantity helper funcs
addQuantitiesToBimap :: QuantityBimap -> Quantities -> QuantityBimap
addQuantitiesToBimap = foldl (\qBimap (quantity, dimVec) -> Bimap.insert quantity dimVec qBimap)

-- Units
newtype Unit = UnitC [(String, Integer)] deriving (Eq, Ord)

instance Show Unit where
    show (UnitC units) = List.intercalate "." (map showUnit units)
      where
        showUnit (baseName, index) = baseName ++ show index

-- Unit helper funcs

-- need to filter dups, process indices, and define ordering
mkUnit :: SrcUnit -> Unit
mkUnit base@[(baseName, 1) ]= UnitC base
mkUnit units = UnitC . Map.toList . foldl mkUnit' Map.empty $ units
  where
    mkUnit' unitMap (unitName, index) = Map.insertWith' (+) unitName index unitMap

isBaseUnit :: Unit -> Bool
isBaseUnit (UnitC [(baseName, 1)]) = True
isBaseUnit _ = False

type SrcUnit = [(SrcId, Integer)]

-- hold this temp structure in indiv module, and promote to global state (UnitDimEnv) when imported & processed
data UnitDef :: * where
    BaseUnitDef :: Unit -> DimVec -> UnitDef
    DerivedUnitDef :: Unit -> UnitDef
    deriving (Eq, Ord, Show)

-- type BaseUnitMap = Map.Map Unit Integer
--type UnitAlias = String
--type UnitAliasBimap = Bimap.Bimap UnitAlias Unit

-- TODO - can these structures be simplified/unified
-- mapping from (base?) units to dimensions
type UnitDimEnv = Map.Map Unit DimVec

-- Unit Env helper funcs
-- calculate the dimension of a given derived unit
calcUnitDim :: Unit -> UnitDimEnv -> MExcept DimVec
calcUnitDim u@(UnitC units) unitEnv = mconcat <$> mapM getDim units
  where
    getDim (name, index) = case Map.lookup (mkUnit [(name,1)]) unitEnv of
        Nothing -> throwError $ printf "Reference to unknown base unit %s found in %s" name (show u)
        Just dim -> return $ mulDim dim index

addUnitsToEnv :: [UnitDef] -> UnitDimEnv -> MExcept UnitDimEnv
addUnitsToEnv units unitEnv = DF.foldlM addUnit unitEnv units
  where
    addUnit unitEnv (BaseUnitDef u d) = case Map.lookup u unitEnv of
        Nothing -> return $ Map.insert u d unitEnv
        Just _ -> throwError $ printf "Base unit %s already defined" (show u)
    addUnit unitEnv (DerivedUnitDef u) = case Map.lookup u unitEnv of
        Nothing -> Map.insert u <$> (calcUnitDim u unitEnv) <*> pure unitEnv
        Just _ -> throwError $ printf "Derived unit %s already defined" (show u)


-- conversion factors between units of same dim
data ConvDef = ConvDef Unit Unit CExpr deriving (Show, Eq, Ord)
type ConvGraph = String
data ConvData = ConvData ConvGraph
type ConvEnv = Map.Map DimVec ConvData

-- | restricted expression AST for conversion functions
-- where ConvFromId is the static identifier for the source unit value
data CExpr = CExpr COp CExpr CExpr | CNum Double | CFromId deriving (Show, Eq, Ord)
data COp = CAdd | CSub | CMul | CDiv deriving (Show, Eq, Ord)

--type ConversionFactor =
addConvsToGraph :: [ConvDef] -> ConvEnv -> ConvEnv
addConvsToGraph = undefined
