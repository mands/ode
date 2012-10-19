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
linkScript, runStaticScript
) where

import Shelly
import Data.Text.Lazy as LT

import Utils.CommonImports
import Subsystem.Simulation.Common
import Subsystem.Simulation.JITCompiler.JITCommon
import Subsystem.Simulation.JITCompiler.JITModel
default (LT.Text)


linkScript :: Sh ()
linkScript = do
    liftIO $ debugM "ode3.sim" $ "Starting LLVM Linker Script"

    --res <- errExit False $ run "opt" ["--version"]
    --echo =<< (toTextIgnore <$> canonic (fromText "./model.bc"))

    -- delete the old sim file
    rm_f "./sim.bc"

    -- optimise the model
    run "opt" ["-o", modelPath, "-std-compile-opts", modelPath]
    -- link the model to stdlib
    run "llvm-link" ["-o", simPath, modelPath, toTextIgnore libPath]
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
runStaticScript :: Sh ()
runStaticScript = do
    liftIO $ debugM "ode3.sim" $ "Starting Run Static Script"

    -- delete the old sim file
    rm_f output

    -- use clang to statically link our llvm-linked sim module to the system
    run "clang" $ ["-static", "-o", toTextIgnore output, "-O3", simPath] ++ libDir ++ libs

    -- execute the file
    run output []

    return ()
  where
    output      = "./sim.exe"
    simPath     = "./sim.bc"
    libs        = ["-lm"]
    libDir      = []
