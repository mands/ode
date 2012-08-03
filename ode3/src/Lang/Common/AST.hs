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
-- | common AST Types, ADTs, etc. across Ode, Core, and CoreANF
--
-----------------------------------------------------------------------------

module Lang.Common.AST (
    -- common id types
    NumTy, SrcId, DesId, Id,

    -- modules
    ModImport(..), ModURIElems, ModRoot, ModName(..), ModFullName(..),
    mkModRoot, getModRootStr, mkModFullName,splitModFullName,

    -- reexport some common units stuff
    U.DimVec(..), U.SrcUnit, U.CExpr(..), U.COp(..)
) where

import qualified Data.List as List
import qualified Lang.Core.Units as U
import Utils.Utils

-- Shared Identifiers --------------------------------------------------------------------------------------------------

-- | an individual number type, not sure if needed, used to convert from double to integer
type NumTy = Double

-- TODO - change to GADT/kind-level
-- | Identifier - basicially RdrName - needs to become parameterised, hold the id and the optional unit annotation
type SrcId = String
-- type DesId = (SrcId, Maybe UnitT)
type DesId = String
type UntypedId = Int
type Id = Int

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













