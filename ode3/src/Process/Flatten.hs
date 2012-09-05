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

import Control.Monad.State
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

flatten :: String -> SysExcept ACF.Module
flatten initModStr = do
    -- lookup the refmod in repl filedata
    replFD <- getSysState vLocalFile
    initMod <- lift $  maybeToExcept (Map.lookup (ModName initModStr) (fileModEnv replFD))
                        (printf "Cannot find module %s loaded to simulate" (show initModStr))
    trace' [MkSB initMod] "CoreFlat AST input" $ return ()
    -- inline mods
    gModEnv <- getSysState vModEnv
    mod1 <- lift $ inlineMod gModEnv initMod
    trace' [MkSB mod1] "Inline Mods output" $ return ()
    -- inline components
    mod2 <- lift $ inlineComps mod1
    trace' [MkSB mod2] "Inline Comps output" $ return ()
    -- convert units and types
    unitsState <- getSysState lUnitsState
    mod3 <- lift $ convertTypes mod2 unitsState
    -- trace' [MkSB mod3] "Convert units output" $ return ()
    -- convert to CoreFlat
    core1 <- lift $ convertAST mod3
    trace' [MkSB core1] "CoreFlat AST output" $ return ()
    -- unpack tuples (in CoreFlat)
    core2 <- lift $ unpackTuples core1
    trace' [MkSB core2] "Unpacked tuples output" $ return ()
    return core2
