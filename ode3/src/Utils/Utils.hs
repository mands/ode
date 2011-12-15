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

{-#LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

module Utils.Utils (
MExcept, PrettyPrint(..), mapFst, mapSnd, pairM,
SB(..), trace', errorDump
) where

import Control.Monad
import Control.Monad.Error
import Data.List (intercalate)
import Debug.Trace

-- | my exception/error monad, could just import from Control.Monad.Error but anyway...
type MExcept = Either String

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

pairM :: (Monad m) => m a -> m b -> m (a, b)
pairM a b = liftM2 (,) a b

-- TODO - use PrettyPrint class from Platform
-- | pretty-printing class for viewing, not machine-readable like Show
class PrettyPrint a where
    prettyPrint :: a -> String

-- existential wrapper, pass the type and the functions/interface that operates on the type,
-- therefore unify/collect set of types  into a group
data SB :: * where
    MkSB :: Show a => a -> SB

instance Show SB where
    show (MkSB s) = show s

trace' :: [SB] -> String -> a -> a
trace' vars msg res = trace outStr res
  where
    showVars = intercalate "\n" $ map show vars
    outStr = "TRACE - " ++ msg ++ "\n" ++ showVars

errorDump :: [SB] -> String -> a
errorDump vars msg = trace' vars msg (error "ERROR DUMP")
