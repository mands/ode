-----------------------------------------------------------------------------
--
-- Module      :  IonAST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Describes the AST for the ion channel language
--
-----------------------------------------------------------------------------

{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             FunctionalDependencies,
             OverlappingInstances,
             FlexibleInstances,
             UndecidableInstances #-}

module Ion.AST (
IonModel(..), IonChannel(..), Transition(..), Id, mkIonChannel, IonExpr(..), optExpr, IonMatrix, IonVector
) where

import Control.Monad.Error
import qualified Data.Set as Set

import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.Tree
import qualified Utils.Graph as UG
import qualified Data.Array as A
import qualified Utils.OrdMap as OrdMap
import AST.CoreFlat(SimType)
import Data.Monoid
import Utils.Utils

-- |identifier - is converted later on
type Id = String

-- |top level model, maybe switch to map indexed by channel name
type IonModel = [IonChannel]

-- |description of an individual ion channel, containing all relevent information
-- collection of derived data strcutures to descibe the system, need
-- * set of all states
-- * graph describing reactions within system
-- * stoc matrix
-- * ...
data IonChannel = IonChannel    {  name :: Id, states :: Set.Set Id, transitionGraph :: UG.GraphMap Id IonExpr
                                , stocMatrix :: Maybe IonMatrix, wieners :: Maybe [Id], vals :: OrdMap.OrdMap Id IonExpr

                                , simType :: SimType, density :: Double, eqPot :: Double, chanConductance :: Double
                                , subunits :: Integer , inputs :: [Id], initialStates :: [(Id, Double)], openStates :: [Id], transitions :: [Transition]
                                } deriving Show

mkIonChannel = IonChannel "" Set.empty UG.mkGraphMap Nothing Nothing OrdMap.empty


-- |description of the bi-directional state-change reaction within an ion-channel
data Transition = Transition {stateA :: Id, stateB :: Id, fRate :: IonExpr, rRate :: IonExpr} deriving Show

type IonMatrix = A.Array (Int,Int) IonExpr
type IonVector = A.Array Int IonExpr


-- Ion Expressions -----------------------------------------------------------------------------------------------------

-- A little untyped expressions syntax
-- TODO - use GADTs to type it
data IonExpr :: * where
    -- basic vars
    Var :: Id -> IonExpr -- vars that contain lookup only into any known vals (inits, wieners, normal vals)
    Num :: Double -> IonExpr
    ExprMacro :: String -> IonExpr -- a built in macro that represents a (hopefully) valid synttic expression that evals to a float
    -- operators
    Add :: IonExpr -> IonExpr -> IonExpr
    Mul :: IonExpr -> IonExpr -> IonExpr
--    Sub :: IonExpr -> IonExpr -> IonExpr
--    Div :: IonExpr -> IonExpr -> IonExpr
    Neg :: IonExpr -> IonExpr
    Sqrt :: IonExpr -> IonExpr
    deriving (Show, Eq, Ord)

-- basic sum and product monoids for IonExpr
instance Monoid (Product IonExpr) where
    mempty = Product $ Num 1
    Product x `mappend` Product y = Product $ Mul x y

instance Monoid (Sum IonExpr) where
    mempty = Sum $ Num 0
    Sum x `mappend` Sum y = Sum $ Add x y

-- optimise the expression, inc,
optExpr :: IonExpr -> IonExpr
optExpr e = until' optExpr' e
  where
    optExpr' :: IonExpr -> IonExpr
    optExpr' (Mul x y)  | isZero x = Num 0              -- Mul x 0 = 0,
                        | isZero y = Num 0
                        | isOne x = optExpr' y          -- Mul x 1 = x
                        | isOne y = optExpr' x
                        | isNegOne x = Neg $ optExpr' y -- Mul x -1 = -x
                        | isNegOne y = Neg $ optExpr' x
    -- * Add x 0 = x
    optExpr' (Add x y)  | isZero x = optExpr' y
                        | isZero y = optExpr' x
    -- misc - are these needed? most unopts exprs come from monoid ops
    optExpr' (Neg (Num n)) = Num $ negate n
    -- basic recursive exprs
    optExpr' (Mul x y) = Mul (optExpr' x) (optExpr' y)
    optExpr' (Add x y) = Add (optExpr' x) (optExpr' y)
--    optExpr' (Sub x y) = Sub (optExpr' x) (optExpr' y)
--    optExpr' (Div x y) = Div (optExpr' x) (optExpr' y)
    optExpr' (Neg x) = Neg (optExpr' x)
    optExpr' (Sqrt x) = Sqrt (optExpr' x)
    optExpr' e = e
    -- guards
    isZero (Num 0) = True
    isZero _ = False
    isOne (Num 1) = True
    isOne _ = False
    isNegOne (Num (-1)) = True
    isNegOne _ = False
