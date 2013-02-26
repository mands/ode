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

module AST.Common (
    -- common id types
    NumTy, SrcId, DesId, Id, RecId,

    -- modules
    ModImport(..), ModURIElems, ModRoot, ModName(..), ModFullName(..),
    mkModRoot, getModRootStr, mkModFullName,splitModFullName,

    -- operators
    Op(..), BasicOp(..), MathOp(..), OtherOp(..), -- , opReservedNames

    -- reexport some common units stuff
    U.DimVec(..), U.UnitList, U.BaseUnit, U.CExpr(..), U.COp(..)
) where

import qualified Data.List as List
import qualified Subsystem.Units as U
import Utils.Utils

-- Shared Identifiers --------------------------------------------------------------------------------------------------

-- | an individual number type, not sure if needed, used to convert from double to integer
type NumTy = Double

-- TODO - change to GADT/kind-level
-- | Identifier - basicially RdrName - needs to become parameterised, hold the id and the optional unit annotation
type SrcId = String
-- type DesId = (SrcId, Maybe UnitT)
type DesId = String
type UntypedId = Integer
type Id = Integer
type RecId = String

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

-- Op Definitions -------------------------------------------------------------------------------------------
-- | all built-in operators in the language including,
--
-- * basic numerical ops - add, mult, etc.
-- * logical ops - and, or, etc.
-- * mathematical ops - trig, exp, etc.
--
-- it is basically a hardcoded stdlib that wraps/interfaces to many GNU C functions
-- we manually include the types here, it's basically the Ode FFI
-- TODO - maybe add a general FFI ??
-- Currently suported operations
data Op = BasicOp BasicOp | MathOp MathOp | OtherOp OtherOp deriving (Show, Eq, Ord)

-- basic operators implemented as CPU ops
data BasicOp    =
                -- Basic Ops
                  Add           -- (f, f) -> f
                | Sub           -- (f, f) -> f
                | Mul           -- (f, f) -> f
                | Div           -- (f, f) -> f
                | Mod           -- (f, f) -> f
                -- Relational Ops
                | LT            -- (f, f) -> b
                | LE            -- (f, f) -> b
                | GT            -- (f, f) -> b
                | GE            -- (f, f) -> b
                | EQ            -- (f, f) -> b
                | NEQ           -- (f, f) -> b
                -- logical Ops
                | And           -- (b, b) -> b
                | Or            -- (b, b) -> b
                | Not           -- b -> b
                | Neg           -- Only used within frontend
                deriving (Show, Eq, Ord)

-- these (transcential) ops copied direct from GNU Math library (libm)
data MathOp     =
                -- basic trig
                  Sin           -- f -> f
                | Cos           -- f -> f
                | Tan           -- f -> f
                -- SinCos        -- f -> (f,f) -- removed for now until add support for MRVs within Ops
                -- inverse trig
                | ASin          -- f -> f
                | ACos          -- f -> f
                | ATan          -- f -> f
                | ATan2         -- (f,f) -> f
                -- exponentials
                | Exp           -- f -> f
                | Exp2          -- f -> f
                | Exp10         -- f -> f
                | Pow10         -- f -> f
                -- logs
                | Log           -- f -> f
                | Log2          -- f -> f
                | Log10         -- f -> f
                | LogB          -- f -> f
                -- non-unit powers - remove??
                | Pow           -- (f,f) -> f
                | Sqrt          -- f -> f
                | Cbrt          -- f -> f
                -- specialised, composite, funcs
                | Hypot         -- (f,f) -> f -- same unit
                | ExpM1         -- f -> f
                | Log1P         -- f -> f
                -- hyperbolics
                | SinH          -- f -> f
                | CosH          -- f -> f
                | TanH          -- f -> f
                | ASinH         -- f -> f
                | ACosH         -- f -> f
                | ATanH         -- f -> f
                -- special funcs (not including Bessel funcs.)
                | Erf           -- f -> f
                | ErfC          -- f -> f
                | LGamma        -- f -> f
                -- Gamma        -- f -> f -- not needed, instead both "gamma" and "tgamma" resovle to TGamma
                | TGamma        -- f -> f
                -- basic floating point ops
                | FAbs                   -- f -> f
                | Floor                 -- f -> f
                | Ceil                  -- f -> f
                | Round                 -- f -> f
                deriving (Show, Eq, Ord)

-- custom operations
data OtherOp    =
                -- random numbers (assume automatic seeding)
                Rand                    -- None -> f (0 < f < 1)
                | UPow Integer          -- f -> f
                | URoot Integer         -- f -> f
                deriving (Show, Eq, Ord)

