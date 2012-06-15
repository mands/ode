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
    DimVec(..), Unit(..), addDim, subDim, mkDimVec, dimensionless, isZeroDim
) where

import qualified Data.List as List
import qualified Data.List.Split as ListSplit
import qualified Data.Bimap as Bimap
import qualified Data.Map as Map


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

-- Units
type Quantity = String

-- structure to hold dimenstion vector
-- Dim L M T I O J N
data DimVec = DimVec    { dimL :: Integer, dimM :: Integer, dimT :: Integer, dimI :: Integer
                        , dimO :: Integer, dimJ :: Integer, dimN :: Integer
                        } deriving (Show, Eq, Ord)

mkDimVec = DimVec 0 0 0 0 0 0 0
dimensionless = DimVec 0 0 0 0 0 0 0

-- helper funcs
addDim (DimVec l m t i o j n) (DimVec l' m' t' i' o' j' n') =
    DimVec (l+l') (m+m') (t+t') (i+i') (o+o') (j+j') (n+n')

subDim (DimVec l m t i o j n) (DimVec l' m' t' i' o' j' n') =
    DimVec (l-l') (m-m') (t-t') (i-i') (o-o') (j-j') (n-n')

isZeroDim = (==) dimensionless

negDim (DimVec l m t i o j n) = DimVec (negate l) (negate m) (negate t) (negate i) (negate o) (negate j) (negate n)


type QuantityBimap = Bimap.Bimap Quantity DimVec
type Quantities = [(Quantity, DimVec)]

-- TODO - fix unit def
-- Dim, Name/Map, Alias

data Unit :: * where
    BaseUnit :: DimVec -> SrcId -> (Maybe String) -> Unit
    DerivedUnit :: DimVec -> (Map.Map SrcId Integer) -> (Maybe String) -> Unit
    deriving (Eq, Ord, Show)

type UnitBimap = Bimap.Bimap String Unit

-- type BaseUnitMap = Map.Map Unit Integer





