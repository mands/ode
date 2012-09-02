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


flatten :: String -> SysExcept ()
flatten initModStr = do
    -- lookup the refmod in repl filedata
    replFD <- getSysState vLocalFile
    initMod <- lift $  maybeToExcept (Map.lookup (ModName initModStr) (fileModEnv replFD))
                        (printf "Cannot find module %s loaded to simulate" (show initModStr))

    trace' [MkSB initMod] "InitMod" $ return ()

    gModEnv <- getSysState vModEnv
--    tmpMod <- lift $ getModuleGlobal modFullName gModEnv
--    trace' [MkSB tmpMod] "CoreFlat AST input" $ return ()

    -- inline mods
    -- mod1 <- lift $ inlineMod gModEnv initMod
    -- trace' [MkSB mod1] "Inline Mods output" $ return ()


    -- flatten all nested lets
--    mod1 <- lift $ flattenExprs tmpMod
    --trace' [MkSB mod1] "Flatten exprs output" $ return ()

    -- inline components
    --mod2 <- lift $ inlineComps tmpMod
    -- trace' [MkSB mod2] "Inline Comps output" $ return ()

    -- TODO - expand tuples and recs

    -- convert units and types
    -- TODO - tmp/dummy module here to fool later stages
--    unitsState <- getSysState lUnitsState
--    mod3 <- lift $ convertTypes mod2 unitsState
--    trace' [MkSB mod3] "Convert units output" $ return ()

    -- convert to CoreFlat
--    coreFlatMod <- lift $ convertAST mod3
--    trace' [MkSB coreFlatMod] "CoreFlat AST output" $ return ()


    return ()


-- Functor Application Helper Funcs ------------------------------------------------------------------------------------
-- TODO - utilise this code later when we unfold all modules into a single block of code
