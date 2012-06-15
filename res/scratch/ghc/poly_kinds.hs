{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures, DataKinds, StandaloneDeriving, FlexibleInstances #-}

-- uses data kinds - arg to S must be of kind P
data P = Z | S P

-- DataKinds test --
-- promoted Kinds
data UnitKind = BUnit | DUnit deriving (Show)

-- default GADT with Typed-Kind Signature, we can't create UnitK Int, kind must be UnitType
data UnitK :: UnitKind -> * where
    BaseUnitK ::  UnitK BUnit
    DerivedUnitK ::  UnitK DUnit
    -- deriving (Eq, Ord, Show)

--deriving instance Show (UnitK UnitKind)
--deriving instance Show (UnitK DUnit)

instance Show (UnitK 'DUnit) where
  show a = "DUnit"

instance Show (UnitK 'BUnit) where
  show a = "BUnit"

-- restricutred function pattern-match - will only take BaseUnitKs, and throw erro on any other type
funK :: UnitK BUnit -> String
funK BaseUnitK = show "base"
-- testF DerivedUnit = show "der"

-- generic fucntion pattern-match - will allow any type of UnitK with pattern-match
funK2 :: UnitK a -> String
funK2 BaseUnitK = show "base"
funK2 DerivedUnitK = show "der"

-- unitK is restricted to types with Kind UnitKind
-- funK3 :: UnitK Int -> String
-- funK3 _ = show "invalid"

-- can only call restricutred testK with BaseUnitK - all others are type-errors
valK1 = funK BaseUnitK
-- valK2 = funK dUnitK


-- collections

-- a generic list - we can't create this
type ListUnitK a = [UnitK a]
-- type LUnitK1 = [UnitK UnitType]

-- cannot create a list/map/etc of general UnitK 
--listK2 ::  [UnitK a]
--listK2 = [BaseUnitK, BaseUnitK]

listK3 ::  [UnitK BUnit]
listK3 = []


-- existential wrapper
--data UnitT = forall n. T (UnitK n)

-- GADT existential wrapper
data UnitT2 where
  T2 :: UnitK n -> UnitT2

listK4 :: [UnitT2]
listK4 = [T2 BaseUnitK, T2 DerivedUnitK]

-- doesn't work - think cos of laziness and empty data delc
funK4 :: UnitT2 -> UnitK BUnit
funK4 (T2 BaseUnitK) = BaseUnitK
funK4 (T2 DerivedUnitK) = undefined

funK5 :: UnitT2 -> Maybe (UnitK BUnit)
funK5 (T2 BaseUnitK) = Just BaseUnitK
funK5 (T2 DerivedUnitK) = Nothing

--funK6 :: UnitT -> UnitK UnitKind
--funK6 (T BaseUnitK) = BaseUnitK
--funK6 (T DerivedUnitK) = DerivedUnitK

