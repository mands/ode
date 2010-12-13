-----------------------------------------------------------------------------
--
-- Module      :  Simulate
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Controls the simualtion process of the model by calling out to UInterpret for indivdual runs
-- | Runs in the IO monad
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Simulate (
startSim
) where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import System.Random
import System.IO

import AST
import UInterpret
import Utilities
import ExecutableAST


-- how do we start, maybe use the simulate functions
-- model exists as a set of typed functions
startSim :: Model -> MExcept (IO ())
startSim (Model sims fenv oenv) = return $ do
    -- print fenv
    -- for each simulate statement, get the inital function from the model and call it
    mapM_ (runSim fenv oenv) sims

runSim :: FunEnv -> OdeEnv -> Simulate -> IO ()
runSim fenv oenv (Simulate comp param from to step sample filename) = do
    putStrLn "Starting Simulation"
    --print oenv
    -- get a rand stdgen from IO here...
    randS <- newStdGen
    -- or, deterministic sequence of rands...
    -- let randS = mkStdGen 12342
    -- open the output file and set sample rate
    hFile <- openFile filename WriteMode
    hSetBuffering hFile (BlockBuffering Nothing)
    -- strictly fold over time (!) evaluating the entry function at each point
    _ <- foldl' (oneStep hFile) (return (sample, (IntState oenv randS Nothing))) time
    -- all done
    hClose hFile
    putStrLn "Done Simulation"
  where
    -- get the entry function
    entryF = fromJust $ Map.lookup comp fenv
    -- create our time list
    time = [from,step..to]

    -- performs a simgle time step interpretation
    -- need to call the entry function with the param "time"
    -- during onestep algorithm collect data into writer monad such that we can write to disk later
    oneStep :: Handle -> IO (Integer, IntState) -> NumTy -> IO (Integer, IntState)
    oneStep hFile ioS t = do
        (curSamp, state) <- ioS
        let (state', output) = runMyInterpret (evalFun entryF [t]) state (Env fenv [] step)
        -- need to write output to file, quick hack to remove the sqaure brackets
        let output' = showsValOut output (show t)
        --  putStrLn output' >>
        curSamp' <- if (curSamp == sample)
            then (hPutStrLn hFile output' >> return 1) else return (curSamp + 1)
        -- print $ head vals
        return (curSamp', state')

-- a simple printer for output values
showsValOut :: [NumTy] -> ShowS
showsValOut [] = id
showsValOut (x:xs) = shows x . (',':).showsValOut xs

{-
instance Show [NumTy] where
    showsPrec _ x = showsValOut x
-}
