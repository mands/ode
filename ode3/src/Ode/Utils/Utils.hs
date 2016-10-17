-----------------------------------------------------------------------------
--
-- Module      :  Utilities
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |General, reusable utilities go here
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures, FlexibleContexts #-}

module Ode.Utils.Utils (
MExcept, MExceptIO, mkExceptIO, maybeToExcept, maybeToExcept', maybeToExceptIO,
mapFst, mapSnd, mapFstM, mapSndM, pairM, notEqual, inc, dec, isWholeNumber, quot', quotRem', rem',
(|>),
SB(..), trace', errorDump,
openPipe, closePipe, readLoop,
mkLabelName, capitalise,
listUniqs,
mapArrayWithIdx, matMatMult, matVecMult, genDiagMat, initMat,
until'
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error
import qualified Data.List as List
import Debug.Trace
import qualified System.IO as SIO
import qualified Data.Bimap as Bimap
import Data.Char (toUpper)
import Data.List (nub)
import GHC.Base(assert)
import Text.Show.Pretty(ppShow)
import Data.Maybe (fromJust)
import qualified Data.Array as A
import Data.Monoid

-- Misc Functions ------------------------------------------------------------------------------------------------------
-- basic piping/chaining of functions
(|>) :: a -> (a->b) -> b
(|>) x f = f x
infixl 0 |>


-- Useful Tuple helpers
mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x,y) = (f x,y)

mapFstM :: (Monad m) => (a -> m b) -> (a, c) -> m (b, c)
mapFstM f (x,y) = liftM2 (,) (f x) (return y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x,y) = (x,f y)

mapSndM :: (Monad m) => (a -> m b) -> (c, a) -> m (c, b)
mapSndM f (x,y) = liftM2 (,) (return x) (f y)

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

-- Math Funcs
a `notEqual` b = not (a == b)

inc = (+) 1
dec = (-) 1

isWholeNumber :: Double -> Bool
isWholeNumber n = (fromInteger $ floor n) == n

-- | generalisation of 'quot' to any instance of Real
quot' :: (Real a, Integral b) => a -> a -> b
quot' n d = truncate ((toRational n) / (toRational d))

-- | generalisation of 'quotRem' to any instance of Real
quotRem' :: (Real a, Integral b) => a -> a -> (b, a)
quotRem' n d = (f, n - (fromIntegral f) * d)
  where
    f = quot' n d

-- | generalisation of 'rem' to any instance of Real
rem' :: (Real a) => a -> a -> a
rem' n d = n - (fromIntegral f) * d
  where
    f = quot' n d


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

-- partial version
maybeToExcept' :: Maybe a -> MExcept a
maybeToExcept' = return . fromJust

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
    showVars = List.intercalate "\n" $ map ppShow vars
    outStr = "TRACE - " ++ msg ++ "\n" ++ showVars

-- | Terminates the program while printing the message and list of vars
-- errorDump :: [SB] -> String -> (Bool -> a -> a) -> a
errorDump vars msg f = (trace' vars ("Internal Compiler Error\n" ++ msg) (f False)) undefined

-- Bimap Ord Instance
instance (Ord a, Ord b) => Ord (Bimap.Bimap a b) where
    compare bx by = compare (Bimap.toAscList bx) (Bimap.toAscList by)


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
mkLabelName s = 'l' : capitalise s

-- | Capitalise the first char of a string
capitalise :: String -> String
capitalise [] = []
capitalise (x:xs) = toUpper x : xs

-- Array Functions -----------------------------------------------------------------------------------------------------

-- convert to a assoclist first
mapArrayWithIdx :: A.Ix i => (i -> e -> f) -> A.Array i e -> A.Array i f
mapArrayWithIdx f arr = A.listArray (A.bounds arr) $ map (uncurry f) (A.assocs arr)

-- generic matrix multiplication, (based on sample from http://www.haskell.org/tutorial/arrays.html and RossetaCode)
matMatMult :: (A.Ix i, Monoid (Sum a), Monoid (Product a)) => A.Array (i,i) a -> A.Array (i,i) a -> A.Array (i,i) a
matMatMult x y
    | x1 /= y0 || x1' /= y0'  = error "range mismatch"
    | otherwise               = A.array ((x0,y1),(x0',y1')) l
  where
    ((x0,x1),(x0',x1')) = A.bounds x
    ((y0,y1),(y0',y1')) = A.bounds y
    ir = A.range (x0,x0')
    jr = A.range (y1,y1')
    kr = A.range (x1,x1')
    l  = [((i,j), getSum $ mconcat [Sum . getProduct $ mappend (Product $ x A.! (i,k)) (Product $ y A.! (k,j)) | k <- kr]) | i <- ir, j <- jr]


matVecMult :: (A.Ix i, Monoid (Sum a), Monoid (Product a)) => A.Array (i,i) a -> A.Array i a -> A.Array i a
matVecMult x v
    | x1 /= v0 || x1' /= v0'  = error "range mismatch"
    | otherwise               = A.array (x0,x0') l
  where
    ((x0,x1),(x0',x1')) = A.bounds x
    (v0,v0') = A.bounds v
    ir = A.range (x0,x0')
    kr = A.range (x1,x1')
    l  = [(i, getSum $ mconcat [Sum . getProduct $ mappend (Product $ x A.! (i,k)) (Product $ v A.! k) | k <- kr]) | i <- ir]

-- Geneate a diagonal matrix from a list of elements, impiles Int indexing statring from 1
genDiagMat :: a -> [a] -> A.Array (Int, Int) a
genDiagMat def elems = (initMat i i def) A.// [((j,j), diagVec A.! j) | j <- [1..i]]
  where
    i = length elems
    diagVec = A.listArray (1, i) elems

-- TODO - genMatAdd

-- | Generate an empty 2d matrix of size n
initMat :: Int -> Int -> e -> A.Array (Int, Int) e
initMat rows cols x = A.listArray ((1,1), (rows,cols)) $ repeat x

-- a varient of until that utilise a eq rather than a predicate
until' :: (Eq a) => (a -> a) -> a -> a
until' f x = let x' = f x in
    if x == x' then x else until' f x'

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys

-- a alt. varient of until' (taken from stackoverflow)
simplify f x = converge (==) (iterate f x)
