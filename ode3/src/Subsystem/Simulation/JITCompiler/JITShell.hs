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

llvmLinkScript :: Sys.SimParams -> Sh ()
llvmLinkScript p = do
    liftIO $ debugM "ode3.sim" $ "Starting LLVM Linker Script"

    -- delete the old sim file
    rm_f "./sim.bc"

    -- optimise the model
    if (L.get Sys.lOptimise p)
        then run "opt" ["-o", modelPath, modelOpts, modelPath] >> return ()
        else return ()
    -- link the model to stdlib
    run "llvm-link" ["-o", simPath, modelPath, toTextIgnore libPath]
    -- DEBUG - dis-assemble sim.bc
    run "llvm-dis" [simPath]
    -- perfrom LTO
    if (L.get Sys.lOptimise p)
        then run "opt" ["-o", simPath, linkOpts, simPath] >> return ()
        else return ()

    return ()
  where
    -- TODO - fix these paths
    modelPath   = "./model.bc"
    simPath     = "./sim.bc"
    libDir      = "../res/stdlib"
    libPath     = libDir </> "odelibrary.bc"
    modelOpts   = if (L.get Sys.lOptimise p) then "-std-compile-opts" else ""
    linkOpts     = if (L.get Sys.lOptimise p) then "-std-link-opts -std-compile-opts" else ""


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
        Sys.StaticLink -> run "clang" $ ["-static", "-o", toTextIgnore output, optLevel, simPath] ++ libDir ++ libs
        Sys.DynamicLink -> run "clang" $ ["-o", toTextIgnore output, optLevel, simPath] ++ libDir ++ libs

    -- execute (as ext. process) if specified
    if (L.get Sys.lExecute p) then run output [] >> return () else return ()
    return ()
  where
    output      = "./sim.exe"
    simPath     = "./sim.bc"
    libs        = ["-lm"]
    libDir      = []
    optLevel = if (L.get Sys.lOptimise p) then "-O3" else "-O0"
