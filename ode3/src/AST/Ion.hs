-----------------------------------------------------------------------------
--
-- Module      :  IonAST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Describes the AST for the ion channel language
--
-----------------------------------------------------------------------------

module AST.Ion (
IonModel(..), IonChannel(..), StateReaction(..), Id, mkIonChannel
) where

import Control.Monad.Error
import qualified Data.Set as Set

import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.Tree
import qualified Utils.Graph as UG

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
data IonChannel = IonChannel    {  name :: Id, species :: Set.Set Id, reactionGraph :: UG.GraphMap Id Double
                                , density :: Double, eqPot :: Double, subunits :: Integer
                                , initialState :: Id, openStates :: [Id], states :: [StateReaction]
                                } deriving Show

mkIonChannel = IonChannel "" Set.empty UG.mkGraphMap


-- |description of the bi-directional state-change reaction within an ion-channel
data StateReaction  = StateReaction {stateA :: Id, stateB :: Id, fRate :: Double, rRate :: Double} deriving Show

