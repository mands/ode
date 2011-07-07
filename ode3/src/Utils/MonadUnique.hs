-----------------------------------------------------------------------------
--
-- Module      :  Utils.MonadUnique
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |Simple implementation of Unique monad - taken from haskellwiki
-- a wrapper around State monad
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fglasgow-exts #-}

module Utilities.MonadUnique
        ( UniqueT,
          Unique,
          MonadUnique,
          fresh,
          evalUniqueT,
          evalUnique )
    where

import Control.Monad
import Control.Monad.State
import Control.Monad.Identity

newtype UniqueT m a = UniqueT (StateT Integer m a)
    deriving (Functor, Monad, MonadTrans, MonadIO)

newtype Unique a = Unique (UniqueT Identity a)
    deriving (Functor, Monad, MonadUnique)

class Monad m => MonadUnique m where
    fresh :: m Integer

instance (Monad m) => MonadUnique (UniqueT m) where
    fresh = UniqueT $ do
                n <- get
                put (succ n)
                return n

evalUniqueT (UniqueT s) = evalStateT s 0
evalUnique (Unique s) = runIdentity (evalUniqueT s)
