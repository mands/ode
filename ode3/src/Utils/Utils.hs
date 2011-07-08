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
MExcept, PrettyPrint(..)
) where

import Control.Monad
import Control.Monad.Error

-- |my exception/error monad, could just import from Control.Monad.Error but anyway...
type MExcept = Either String

-- |convenience func for threading a single Maybe through a fold
-- like mapMaybe but for a single maybe
-- again is a freaking monad(plus) operation!!
foldlMaybe :: (a -> Maybe b) -> [a] -> Maybe b
--foldlMaybe f xs = foldl (\res x -> case res of Just _ -> res; Nothing -> f x) Nothing xs
foldlMaybe f xs = msum (map f xs)

-- |sets up a chain of either calculations, passing the first error encoutered along
-- this is a freaking monad!! :) - the Maybe or Either/Error Monad!!
runEither :: (b -> Either a c) -> Either a b -> Either a c
runEither f val = either (\err -> Left err) (\res -> f res) val

-- |pretty-printing class for viewing, not machine-readable like Show
class PrettyPrint a where
    prettyPrint :: a -> String
