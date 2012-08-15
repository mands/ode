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

{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures, FlexibleContexts #-}

module Utils.Utils (
MExcept, MExceptIO, mkExceptIO, maybeToExcept, maybeToExceptIO,
mapFst, mapSnd, pairM,
(|>),
SB(..), trace', errorDump,
openPipe, closePipe, readLoop,
mkLabelName,
listUniqs
) where

import Control.Monad
import Control.Monad.Error
import qualified Data.List as List
import Debug.Trace
import qualified System.IO as SIO
import Data.Bimap
import Data.Char (toUpper)
import Data.List (nub)
import GHC.Base(assert)


-- Misc Functions ------------------------------------------------------------------------------------------------------
-- basic piping/chaining of functions
(|>) :: a -> (a->b) -> b
(|>) x f = f x
infixl 0 |>

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)
mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

pairM :: (Monad m) => m a -> m b -> m (a, b)
pairM a b = liftM2 (,) a b

-- test if all the elements within a list are unique
listUniqs :: (Eq a) => [a] -> Bool
listUniqs xs = length xs == (length . nub) xs

-- | util function to split a list based on predicate function p
splitList :: (a -> Bool) -> [a] -> [[a]]
splitList p xs = h : t'
  where
    (h, t) = List.break p xs
    t' = case t of x:xs -> splitList p xs
                   [] -> []


-- Monadic Error Handling ----------------------------------------------------------------------------------------------
-- Switch to errors package
-- | my exception/error monad, could just import from Control.Monad.Error but anyway...
type MExcept = Either String
type MExceptIO = ErrorT String IO

-- | (un-)lift a monad from MExcept to MExceptIO
mkExceptIO :: MExcept a -> MExceptIO a
mkExceptIO = ErrorT . return

maybeToExcept :: Maybe a -> String -> MExcept a
maybeToExcept m str = case m of
                        Nothing -> throwError str
                        Just x -> return x


mExceptToError :: (MonadError String m) => MExcept a -> m a
mExceptToError (Left err) = throwError err
mExceptToError (Right res) = return res

maybeToExceptIO m = mkExceptIO . maybeToExcept m


-- Error Output --------------------------------------------------------------------------------------------------------
-- existential wrapper, pass the type and the functions/interface that operates on the type,
-- therefore unify/collect set of types  into a group
data SB :: * where
    MkSB :: Show a => a -> SB

instance Show SB where
    show (MkSB s) = show s

-- | Wrapper around Debug.trace that outputs a msg and list of vars
trace' :: [SB] -> String -> a -> a
trace' vars msg res = trace outStr res
  where
    showVars = List.intercalate "\n" $ map show vars
    outStr = "TRACE - " ++ msg ++ "\n" ++ showVars

-- | Terminates the program while printing the message and list of vars
-- errorDump :: [SB] -> String -> (Bool -> a -> a) -> a
errorDump vars msg f = (trace' vars ("Internal Compiler Error\n" ++ msg) (f False)) undefined

-- Bimap Ord Instance
instance (Ord a, Ord b) => Ord (Bimap a b) where
    compare bx by = compare (toAscList bx) (toAscList by)


-- File Piping --------------------------------------------------------------------------------------------------------
-- some helper routines to open a file/named pipe for input
openPipe :: FilePath -> IO SIO.Handle
openPipe inName = do
    -- open up the named pipe
    -- pExists <- (PF.fileExist debugPipe)
    -- pStatus <- (PF.getFileStatus debugPipe)
    -- putStrLn $ "Exists - " ++ show pExists
    -- putStrLn  $ "Named Pipe - " ++ show (PF.isNamedPipe pStatus)
    hCmdPipe <- SIO.openFile inName SIO.ReadMode
    SIO.hSetBuffering hCmdPipe SIO.NoBuffering
    hshow <- SIO.hShow hCmdPipe
    putStrLn  $ "Handle - " ++ hshow
    return hCmdPipe


closePipe :: SIO.Handle -> IO ()
closePipe = SIO.hClose

-- |read lines from the pipe until EOF
readLoop :: SIO.Handle -> IO ()
readLoop hCmdPipe = forever (SIO.hGetLine hCmdPipe >>= outCmd)
  where
    outCmd s = putStrLn s >> SIO.hFlush SIO.stdout

-- FCLabels Functions --------------------------------------------------------------------------------------------------
-- | fclabels fucntion to create labels of form lRecordName
mkLabelName :: String -> String
mkLabelName s = 'l' : toUpper (head n) : tail n
  where
    n = drop 1 s

