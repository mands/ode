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

module Ion.AST (
IonModel(..), IonChannel(..), StateReaction(..), Id
) where

import Control.Monad.Error

-- |identifier - is converted later on
type Id = String

-- |top level model, maybe switch to map indexed by channel name
type IonModel = [IonChannel]

-- |description of an individual ion channel, containing all relevent information
data IonChannel = IonChannel {  name :: Id, density :: Double, equilibrium_potential :: Double, subunits :: Integer,
                                open_states :: [Id], states :: [StateReaction]}
                deriving Show

-- |description of the state-change reaction within an ion-channel
-- we only consider uni-directional reactions for now
data StateReaction  = StateReaction {stateA :: Id, stateB :: Id, rate :: Double}
                    deriving Show
