{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Control.Monad
import Control.Monad.State
-- import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.Trans

-- as new type, derving MonadTrans is only for new transformers yourself
-- lift is defined for all others
newtype TestNT m a = TestNTC (StateT Int m a)
    deriving (Monad, MonadTrans, MonadState Int)

-- this works fine - but need to apply the data constructor TestC after the lift to make sure it's right type
newtype Test a = TestC (StateT Int (Either String) a)
    deriving (Monad, MonadState Int, MonadError String)

testCode :: Test ()    
testCode = do
    a <- put 2
    throwError "Hello"
    return ()

-- auto lift
testCode' :: Test ()    
testCode' = do
    a <-  TestC $ lift $ Left "Hello"
    return ()

-- manual lift
--testCode2 :: Test ()
--testCode2 = do
--    a <- lift $ Left "Hello"
--    return ()

--instance MonadTrans (StateT a (Either b) c) where
--    lift m = case m of
--        Left err -> throwError err
--        Right res -> return res


-- this works fine, lift is defined within mtl, so no probs
type Test1 = StateT Int (Either String)
type MExcept = Either String

testCode1 :: Test1 ()    
testCode1 = do
    a <- put 2
    throwError "Hello"
    return ()

-- auto lift
testCode1' :: Test1 ()    
testCode1' = do
    a <-  lift $ Left "Hello"
    return ()

-- generic func type
testCodeF1 :: (MonadError String m) => m ()
testCodeF1 = do
  throwError "Hello"
  _ <- mExceptToError $ testCodeF2 1
  return ()

testCodeF2 :: Int -> MExcept Int
testCodeF2 i = return (i+1)


mExceptToError :: (MonadError String m) => MExcept a -> m a
mExceptToError (Left err) = throwError err
mExceptToError (Right res) = return res


phi :: Monad m => StateT s m ()
phi = lift $ return () 



