-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE GADTs, EmptyDataDecls, NoMonomorphismRestriction,
MultiParamTypeClasses, KindSignatures, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, FunctionalDependencies  #-}

module Main (
    main
) where

main = do
    putStrLn "Hello World!\n"
    putStrLn "** Test1 **"
    putStrLn (show test1)
    putStrLn "** Test2 **"
    putStrLn (show test2a)
    putStrLn (show test2b)
    putStrLn "** Test3 **"
    putStrLn (show test3)
    putStrLn "** Test4 **"
    putStrLn (show test4a)
    putStrLn (show test4b)
    putStrLn "** Test5 **"
    putStrLn (show test5)


-- GADT version

--data Km
--data Feet

--data Length where
--    MkLength :: Double -> Length deriving Show

--data LengthX where
    --MkFeet :: Double -> LengthX
    --MkKm :: Double -> LengthX

-- consider only length for now
-- basic typeclass default
--class Unit a where
--  convert :: a -> b


--instance Unit Length where
--    convert ::

-- Version 1 - single param TC, Length only
-- explcit conversion required
-- all calcs done in a single chosen unit for a dimension - i.e. M for Length
data M1 where
    MkM1 :: Double -> M1
    deriving (Eq, Show)

instance Num M1 where
  (+) (MkM1 x) (MkM1 y) = MkM1 (x + y)

data Ft1 where
    MkFt1 :: Double -> Ft1
    deriving (Eq, Show)

instance Num Ft1 where
  (+) (MkFt1 x) (MkFt1 y) = MkFt1 (x + y)


class Length1 a where
  convert1 :: a -> M1

instance Length1 M1 where
  convert1 a = a

instance Length1 Ft1 where
  convert1 (MkFt1 f) = MkM1 (f * 0.3048)

testM1 = MkM1 2
testFt1 = MkFt1 3
test1 = testM1 + convert1 testFt1

-- Version 2 - multi-param TC, Length only
-- explcit conversion required
-- can choose/infer the unit automatically for a dimension - i.e. M for Length
class Length2 a b where
  convert2 :: a -> b

instance Length2 Ft1 M1 where
  convert2 (MkFt1 f) = MkM1 (f * 0.3048)

instance Length2 Ft1 Ft1 where
  convert2 = id

instance Length2 M1 Ft1 where
  convert2 (MkM1 m) = MkFt1 (m / 0.3048)

instance Length2 M1 M1 where
  convert2 = id

test2a = testM1 + convert2 testFt1
-- test2b :: Ft1 -- type contraint needed
test2b = convert2 testM1 + testFt1


-- Version 3 - GADT based
-- consider only length for now
-- basic typeclass default
class Dim3 a where
  convert3 :: a -> a

data M3
data Ft3

-- GADT
data Length3 (a :: *) where
    MkM3 :: Double -> Length3 M3
    MkFt3 :: Double -> Length3 Ft3

instance Num (Length3 M3) where
    (+) (MkM3 x) (MkM3 y) = MkM3 (x + y)

instance Show (Length3 M3) where
    show (MkM3 m) = show m

instance Eq (Length3 M3) where
    (==) (MkM3 x) (MkM3 y) = x == y


-- non-GADT - doesn't work, need to differentiat between units - need GADT, Parametric type or typeclass
--data Length3 where
--    MkMetre3 :: Double -> Length3
--    MkFeet3 :: Double -> Length3

-- works but doesn't do anything due to class kind restriction
instance Dim3 (Length3 M3) where
    convert3 (MkM3 m) = MkM3 m
    --convert3 (MkMetre3 m) = MkFeet3 (m / 0.3048)

-- Version 3b - GADT with kind sigs
-- TODO - do we need FunDeps?
class Dim3b (a :: * -> *) b c where
    convert3b :: a b -> a c

instance Dim3b Length3 M3 M3 where
    convert3b (MkM3 m) = MkM3 m

instance Dim3b Length3 M3 Ft3 where
    convert3b (MkM3 m) = MkFt3 (m / 0.3048)

instance Dim3b Length3 Ft3 M3 where
    convert3b (MkFt3 f) = MkM3 (f * 0.3048)

testM3 = MkM3 2
testFt3 = MkFt3 3
test3 = testM3 + convert3b testFt3


-- Version 4 - parameteric parameter, open approarch compared to (G)ADTs
data Length4 (a :: *) where
    MkLength4 :: a -> Double -> Length4 a
    deriving (Show, Eq)

instance (Eq a, Show a) =>  Num (Length4 a) where
    (+) (MkLength4 mkA x) (MkLength4 _ y) = MkLength4 mkA (x + y)

data M4 = MkM4 deriving (Show, Eq)
data Ft4 = MkFt4 deriving (Show, Eq)

class Dim4 (a :: * -> *) b c where
    convert4 :: a b -> a c

instance Dim4 Length4 M4 M4 where
    convert4 (MkLength4 MkM4 m) = MkLength4 MkM4 m

instance Dim4 Length4 Ft4 M4 where
    convert4 (MkLength4 MkFt4 f) = MkLength4 MkM4 (f * 0.3048)

instance Dim4 Length4 M4 Ft4 where
    convert4 (MkLength4 MkM4 m) = MkLength4 MkFt4 (m / 0.3048)


testM4 = MkLength4 (MkM4) 2
testFt4 = MkLength4 (MkFt4) 3
test4a = testM4 + testM4 + convert4 testM4
test4b = testM4 + convert4 testFt4


-- Version 5 - Multi-level type-classes
-- make Length a type-class instead of data constructor, and hold num val inside the particualr unit
data M5 = MkM5 Double deriving (Show, Eq)
instance Num M5 where
    (+) (MkM5 x) (MkM5 y) = MkM5 (x + y)

data Ft5 = MkFt5 Double deriving (Show, Eq)
instance Num Ft5 where
    (+) (MkFt5 x) (MkFt5 y) = MkFt5 (x + y)

class (Num a) => Unit a where
    pack :: Double -> a
    unpack :: a -> Double

class (Unit a) => Length a

instance Unit M5 where
    pack x = MkM5 x
    unpack (MkM5 x) = x

instance Length M5

instance Unit Ft5 where
    pack x = MkFt5 x
    unpack (MkFt5 x) = x

instance Length Ft5

-- should this be a Dim, acts more like a converter between dims, which are Length, etc.
class Dim5 b c where
    convert5 :: b -> c

instance (Length M5, Length Ft5) => Dim5 M5 Ft5 where
    convert5 m = pack (unpack m / 0.3048)

instance (Length Ft5, Length M5) => Dim5 Ft5 M5 where
    convert5 f = pack (unpack f * 0.3048)

testM5 = MkM5 2
testFt5 = MkFt5 3
test5 = testM5 + convert5 testFt5
