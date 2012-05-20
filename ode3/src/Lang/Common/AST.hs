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

module Lang.Common.AST (
    NumTy, UntypedId, Id,
    SrcId, UnitT, DesId,
    ModURIElems, ModRoot(..), ModName(..), ModFullName,
    mkModRoot, mkModFullName,splitModFullName
) where

import qualified Data.List as List
import qualified Data.List.Split as ListSplit

-- | an individual number type, not sure if needed, used to convert from double to integer
type NumTy = Double

type UnitT = String

-- TODO - change to newtype?
-- | Identifier - basicially RdrName - needs to become parameterised, hold the id and the optional unit annotation
type SrcId = String
type UntypedId = Int
type Id = UntypedId
type DesId = (SrcId, Maybe UnitT)


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
newtype ModFullName = ModFullName String deriving (Show, Eq, Ord)

-- data ModImport = ModImport String (Maybe String) deriving (Show, Eq, Ord)

-- smart constructors for ModRoot and ModFullName
-- | Flattens a list of URI elems into dot-notation
mkModRoot :: ModURIElems -> ModRoot
mkModRoot = ModRoot . List.intercalate "."

-- | takes a list of URI elems and modulename into a dot-notation
mkModFullName :: Maybe ModRoot -> ModName -> ModFullName
mkModFullName (Just (ModRoot modURI)) (ModName shortName) = ModFullName $ modURI ++ "." ++ shortName
mkModFullName Nothing (ModName shortName) = ModFullName $ shortName

splitModFullName :: ModFullName -> (Maybe ModRoot, ModName)
splitModFullName (ModFullName fullName) = if length elems == 1 then (Nothing, modName) else (Just modRoot, modName)
  where
    elems = ListSplit.splitOn "." fullName
    modRoot = mkModRoot $ List.init elems
    modName = ModName $ List.last elems



