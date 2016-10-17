-----------------------------------------------------------------------------
--
-- Module      :  Lang.Core.Flatten
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | TODO - main functioalyity that takes a set of Core modules and flattens them into CoreFlat AST
-- involves
-- * inline all modules
-- * inline components
-- * convert all units & drop unneeded type-checking info (wraps/newtypes and units)
-- * convert to ANF and CoreFlat AST

-- Notes
-- use intmap to hold local ids
-- need create a new ADT to hold metadata
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Ode.Process.Flatten (
flatten
) where

import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)

import qualified Data.Traversable as DT
import qualified Data.Map as Map

import Control.Monad.State as S
import Control.Conditional
import Ode.Utils.MonadSupply

import Ode.Utils.CommonImports
import Ode.Subsystem.SysState

import AST.Common
import AST.Module

import qualified Utils.OrdMap as OrdMap
import qualified AST.Core as AC
import qualified AST.CoreFlat as ACF
import qualified Subsystem.Units as U

import Ode.Process.Flatten.InlineMods
import Ode.Process.Flatten.InlineComps
import Ode.Process.Flatten.ConvertAST
import Ode.Process.Flatten.ConvertTypes
import Ode.Process.Flatten.UnpackTuples
import Ode.Process.Flatten.OptimiseAST
import Ode.Process.Flatten.OptimiseCoreFlatAST
import Ode.Process.Flatten.InitialValueGen


flatten :: String -> SysExcept ACF.Module
flatten initModStr = do
    sysSt <- S.get
    -- lookup the refmod in REPL filedata
    replFD <- getSysState vLocalFile
    initMod <- lift $  maybeToExcept (Map.lookup (ModName initModStr) (fileModEnv replFD))
                        (printf "Cannot find module %s loaded to simulate" (show initModStr))
    trace' [MkSB initMod] "Flatten - Initial Core AST input" $ return ()
    -- now run the flatten pipeline in the MExcept monad
    let gModEnv = _modEnv . _modState $ sysSt
    let simParams = _simParams sysSt
    let unitsState = _unitsState sysSt
    let tUnit = _timeUnit simParams -- TODO - need to pass to several filters (optimiseCoreAST, convertAST, convertTypes)
    -- run the mail flatten pipeline
    coreFlatMod <- lift $ inlineMod gModEnv initMod >>= inlineComps >>= convertTypes unitsState tUnit >>= optimiseCoreAST simParams
        >>= initialValueGen simParams >>= convertAST tUnit >>= unpackTuples
    -- TODO - add the optimiseCoreFlatAST?
    trace' [MkSB coreFlatMod] "Flatten - Final CoreFlat AST output" $ return coreFlatMod
    return coreFlatMod


