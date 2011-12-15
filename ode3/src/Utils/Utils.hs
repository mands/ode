-----------------------------------------------------------------------------
--
-- Module      :  Utilities
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |General, reusable utilities go here
--
-----------------------------------------------------------------------------

module Utils.Utils (
MExcept, PrettyPrint(..), mapFst, mapSnd, pairM, errorDump, mkTrace
) where

import Control.Monad
import Control.Monad.Error
import Data.List (intercalate)

-- | my exception/error monad, could just import from Control.Monad.Error but anyway...
type MExcept = Either String

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

pairM :: (Monad m) => m a -> m b -> m (a, b)
pairM a b = liftM2 (,) a b

-- | pretty-printing class for viewing, not machine-readable like Show
class PrettyPrint a where
    prettyPrint :: a -> String


-- TODO - use existentials
errorDump :: [String] -> String
errorDump msgs = "ERROR DUMP \n" ++ (intercalate "\n" msgs)

mkTrace :: [String] -> String
mkTrace msgs = intercalate ", " msgs
