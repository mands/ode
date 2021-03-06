-----------------------------------------------------------------------------
--
-- Module      :  Utils.MonadSupply
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |Simple implementation of Supply monad - taken from haskellwiki
-- a wrapper around State monad, takes a list of values to generate supply
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Ode.Utils.MonadSupply
    (SupplyT,
     MonadSupply,
     supply,
     Supply,
     evalSupplyT,
     evalSupply,
     runSupplyT,
     runSupply)
    where
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Error

newtype SupplyT s m a = SupplyT (StateT [s] m a)
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

newtype Supply s a = Supply (SupplyT s Identity a)
    deriving (Functor, Applicative, Monad, MonadSupply s)

class Monad m => MonadSupply s m | m -> s where
    supply :: m s

instance Monad m => MonadSupply s (SupplyT s m) where
    supply = SupplyT $ do
                (x:xs) <- get
                put xs
                return x

evalSupplyT (SupplyT s) supp = evalStateT s supp
evalSupply (Supply s) supp = runIdentity $ evalSupplyT s supp

runSupplyT (SupplyT s) supp = runStateT s supp
runSupply (Supply s) supp = runIdentity $ runSupplyT s supp
