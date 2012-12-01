-----------------------------------------------------------------------------
--
-- Module      :  Lang.Core.Flatten
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
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

module Process.Flatten (
flatten
) where

import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)

import qualified Data.Traversable as DT
import qualified Data.Map as Map

import Control.Monad.State as S
import Control.Conditional
import Utils.MonadSupply

import Utils.CommonImports
import Subsystem.SysState

import AST.Common
import AST.Module

import qualified Utils.OrdMap as OrdMap
import qualified AST.Core as AC
import qualified AST.CoreFlat as ACF
import qualified Subsystem.Units as U

import Process.Flatten.InlineMods
import Process.Flatten.InlineComps
import Process.Flatten.ConvertAST
import Process.Flatten.ConvertTypes
import Process.Flatten.UnpackTuples
import Process.Flatten.OptimiseAST
import Process.Flatten.OptimiseCoreFlatAST
import Process.Flatten.InitialValueGen


-- TODO - should really clean this up, is so verbose simply so can get debugging output
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
    coreFlatMod <- lift $ inlineMod gModEnv initMod >>= inlineComps >>= convertTypes unitsState >>= optimiseCoreAST simParams
        >>= initialValueGen simParams >>= convertAST >>= unpackTuples
    -- TODO - add the optimiseCoreFlatAST?
    trace' [MkSB coreFlatMod] "Flatten - Final CoreFlat AST output" $ return coreFlatMod

