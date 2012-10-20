-----------------------------------------------------------------------------
--
-- Module      :  Subsystem.Simulation.JITCompiler.JITLink
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | A bit of hackery to link our module together with the stdlib and vecmath implementation
-- NOTE - needs to have a (VecMath enabled) version of LLVM located on the PATH
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Subsystem.Simulation.JITCompiler.JITShell (
llvmLinkScript, llvmAOTScript
) where

-- Labels
import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)


import Shelly
import Data.Text.Lazy as LT

import Utils.CommonImports
import Subsystem.Simulation.Common
import Subsystem.Simulation.JITCompiler.JITCommon
import Subsystem.Simulation.JITCompiler.JITModel
import qualified Subsystem.SysState as Sys

default (LT.Text)

llvmLinkScript :: Sh ()
llvmLinkScript = do
    liftIO $ debugM "ode3.sim" $ "Starting LLVM Linker Script"

    --res <- errExit False $ run "opt" ["--version"]
    --echo =<< (toTextIgnore <$> canonic (fromText "./model.bc"))

    -- delete the old sim file
    rm_f "./sim.bc"

    -- optimise the model
    run "opt" ["-o", modelPath, "-std-compile-opts", modelPath]
    -- link the model to stdlib
    run "llvm-link" ["-o", simPath, modelPath, toTextIgnore libPath]
    -- DEBUG - disassm sim.bcc
    run "llvm-dis" [simPath]
    -- perfrom LTO
    run "opt" ["-o", simPath, "-std-link-opts", simPath]

    return ()
  where
    -- TODO - fix these paths
    modelPath   = "./model.bc"
    simPath     = "./sim.bc"
    libDir      = "../res/stdlib"
    libPath     = libDir </> "odelibrary.bc"


-- | Embedded script to executre a static simulation, utilising static linking to all libs
-- and no call-back to Ode run-time (requires use of Clang and system linker)
llvmAOTScript :: Sys.SimParams -> Sh ()
llvmAOTScript p = do
    liftIO $ debugM "ode3.sim" $ "Starting AOT Script"

    -- delete the old sim file
    rm_f output

    -- check dyn/static linking
    case (L.get Sys.lLinker p) of
        -- use clang to link our llvm-linked sim module to the system
        Sys.StaticLink -> run "clang" $ ["-static", "-o", toTextIgnore output, "-O3", simPath] ++ libDir ++ libs
        Sys.DynamicLink -> run "clang" $ ["-o", toTextIgnore output, "-O3", simPath] ++ libDir ++ libs

    -- execute (as ext. process) if specified
    if (L.get Sys.lExecute p) then run output [] >> return () else return ()
    return ()
  where
    output      = "./sim.exe"
    simPath     = "./sim.bc"
    libs        = ["-lm"]
    libDir      = []
