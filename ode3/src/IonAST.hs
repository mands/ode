-----------------------------------------------------------------------------
--
-- Module      :  IonAST
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Describes the AST for the ion channel language
--
-----------------------------------------------------------------------------

module IonAST (
IonModel(..), IonChannel(..), StateReaction(..), Id
) where

import Control.Monad.Error

-- need to change?
type Id = String

-- can switch within the hpc implemenations as needed, also need bools?
--type NumTy = Double
--type IntTy = Int

-- switch to map indexed by channel name
type IonModel = [IonChannel]

data IonChannel = IonChannel {  name :: Id, density :: Double, equilibrium_potential :: Double, subunits :: Integer,
                                open_states :: [Id], states :: [StateReaction]}
                deriving Show

-- only consider uni-directional reactions for now
data StateReaction  = StateReaction {stateA :: Id, stateB :: Id, rate :: Double}
                    deriving Show
