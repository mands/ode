-----------------------------------------------------------------------------
--
-- Module      :  Utils.MonadSupply
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |Simple implementation of Supply monad - taken from haskellwiki
-- a wrapper around State monad, takes a list of values to genereate supply
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Utils.MonadSupply
    (SupplyT,
     MonadSupply,
     supply,
     Supply,
     evalSupplyT,
     evalSupply,
     runSupplyT,
     runSupply)
    where
import Control.Monad
import Control.Monad.State

newtype SupplyT s m a = SupplyT (StateT [s] m a)
    deriving (Functor, Monad, MonadTrans, MonadIO)

newtype Supply s a = Supply (SupplyT s Maybe a)
    deriving (Functor, Monad, MonadSupply s)

class Monad m => MonadSupply s m | m -> s where
    supply :: m s

instance Monad m => MonadSupply s (SupplyT s m) where
    supply = SupplyT $ do
                (x:xs) <- get
                put xs
                return x

evalSupplyT (SupplyT s) supp = evalStateT s supp
evalSupply (Supply s) supp = evalSupplyT s supp

runSupplyT (SupplyT s) supp = runStateT s supp
runSupply (Supply s) supp = runSupplyT s supp

--runSupplyVars x = runSupply x vars
--    where vars = [replicate k ['a'..'z'] | k <- [1..]] >>= sequence
