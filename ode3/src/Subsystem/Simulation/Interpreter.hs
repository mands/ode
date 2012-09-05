-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Simulation.Interpreter
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Not implemented - should take a CoreFlat AST and simulate it
--
-----------------------------------------------------------------------------

module Subsystem.Simulation.Interpreter (
interpret
) where




import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)

import qualified Data.Foldable as DF
import qualified Data.Traversable as DT
import qualified Data.Map as Map

import Control.Monad.State
import Utils.MonadSupply

import Utils.CommonImports
import qualified Subsystem.SysState as Sys

import AST.Common
import AST.CoreFlat


type SimM = StateT SimState MExceptIO

data SimState = SimState    { simEnv :: Map.Map Id ExprData, curTime :: Float, curPeriod :: Integer, simParams :: Sys.SimParams }
mkSimState = SimState Map.empty 0 0

interpret :: Module -> Sys.SysExceptIO ()
interpret mod = do

    -- setup the default simulation state
    p <- Sys.getSysState Sys.lSimParams
    liftIO $ debugM "ode3.sim" $ "Starting Simulation"
    lift $ runStateT runSimulation $ mkSimState p
    liftIO $ debugM "ode3.sim" $ "Simulation Complete"
    return ()
  where
    runSimulation :: SimM ()
    runSimulation = do
        -- simulate the initial data
        simulateExprMap (initExprs mod)
        -- simulate the loop exprs over the time period
        p <- simParams <$> get
        mapM_ simulateLoop [(L.get Sys.lStartTime p),(L.get Sys.lTimestep p)..(L.get Sys.lEndTime p)]

    -- wrapper function to configure the cur time
    simulateLoop :: Float -> SimM ()
    simulateLoop t = do
        -- set the time
        modify (\st -> st { curTime = t } )
        simulateExprMap (loopExprs mod)


simulateExprMap :: ExprMap -> SimM ()
simulateExprMap exprMap = do
    t <- curTime <$> get
    liftIO $ putStrLn $ printf "In sim, time = %.4f" t


