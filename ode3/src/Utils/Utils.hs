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
MExcept, MExceptIO, PrettyPrint(..), mapFst, mapSnd, pairM,
SB(..), trace', errorDump,
openPipe, closePipe, readLoop,
) where

import Control.Monad
import Control.Monad.Error
import qualified Data.List as List
import Debug.Trace
import qualified System.IO as SIO

-- | my exception/error monad, could just import from Control.Monad.Error but anyway...
type MExcept = Either String
type MExceptIO = ErrorT String IO


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
    showVars = List.intercalate "\n" $ map show vars
    outStr = "TRACE - " ++ msg ++ "\n" ++ showVars

errorDump :: [SB] -> String -> a
errorDump vars msg = trace' vars msg (error "ERROR DUMP")


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



-- | util function to split a list based on predicate function p
splitList :: (a -> Bool) -> [a] -> [[a]]
splitList p xs = h : t'
  where
    (h, t) = List.break p xs
    t' = case t of x:xs -> splitList p xs
                   [] -> []



