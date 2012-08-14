-----------------------------------------------------------------------------
--
-- Module      :  Lang.Core.TypeChecker.Types
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Common types
--
-----------------------------------------------------------------------------

module Lang.Core.TypeChecker.Common
where


import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lang.Core.AST as E
import qualified Lang.Core.Units as U
import qualified Lang.Module.AST as M


-- Global Types ---------------------------------------------------------------------------------------------------------------

-- type TypeEnv = M.TypeMap
-- type env for modules, only one needed as vars should be unique,
-- * for imports copies the type from the global modEnv
-- * for functor holds a type var for the first occurance of a module var
-- holds both local and module-scope types for the module
type TypeEnv = Map.Map (E.VarId E.Id) E.Type

-- Mappings from a type/unit-variable to an actual type
type TypeVarEnv = Map.Map Int E.Type
type UnitVarEnv = Map.Map Int U.Unit


-- collects all references to records within a module (both local and mod level)
type RecordRefMap = Map.Map (E.VarId E.Id) E.Type

-- Constraint Generation -----------------------------------------------------------------------------------------------

-- set of contraints for the module - include both the types and a set of rules that determine the
-- unit level contraints
--data UnitsConRule   = --NoUnit    -- type?/float does not have unit information
----                    | SameUnit  -- floats must be contrained to same unit
--                      Equal     -- Types must be fully equal, regardless if they have units
----                    | SameDim   -- floats must be contrained to same dimenstion, effectively restricted unit-polymorpihism
----                                -- we could add full unit-polymorphism by creating unit-vars for a particular dimenstions and constraining (again) later
--                    deriving (Show, Eq, Ord)

-- true for both types and units ?
data ConType    = ConEqual E.Type E.Type
                -- | ConRecSubType E.Type E.Type -- record t1 is a subtype of record t2
                deriving (Show, Eq, Ord)

data ConUnit    = ConSum U.Unit U.Unit U.Unit   -- can unify Mul&Div into ConsSum (a,b) = c
                | ConSameDim U.Unit U.Unit      -- should this be Unit, not type??
                deriving (Show, Eq, Ord)

type ConTypeS = Set.Set ConType
type ConUnitS = Set.Set ConUnit

data TypeCons = TypeCons    { conTypeS :: ConTypeS
                            , conUnitS :: ConUnitS
                            } deriving (Show, Eq, Ord)

mkTypeCons = TypeCons Set.empty Set.empty
--data ConsRule   = ConsEqual E.Type E.Type       -- true for both types and units ?
--                | ConsSameDim E.Type E.Type     -- should this be Unit, not type??
--                -- ConsMul E.Type E.Type E.Type  -- contrain the results from a multiplcation
--                -- ConsDiv E.Type E.Type E.Type  -- contrain the results from a div
--                -- can unify Mul&Div into ConsSum (a,b) = c
--                | ConsSum (E.Type, E.Type) E.Type
--                -- unit stuff -- can we do at the float level?
--                -- ConsSameUnit Unit Unit     -- is needed?
--                deriving (Show, Eq, Ord)

unitToType :: U.Unit -> E.Type
unitToType = E.TFloat

typeToUnit :: E.Type -> Maybe U.Unit
typeToUnit (E.TFloat u) = Just u
typeToUnit _ = Nothing

