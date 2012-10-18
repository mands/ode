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

module Subsystem.Simulation.JITCompiler.JITLink (
linkModule
) where

import Shelly
import Data.Text.Lazy as LT

import Subsystem.Simulation.Common
import Subsystem.Simulation.JITCompiler.JITCommon
import Subsystem.Simulation.JITCompiler.JITCoreFlat
default (LT.Text)


-- | Calls out to our link script
-- TODO - shoule replace Bash script with direct haskell code
linkModule :: GenM ()
linkModule = do
    shelly . verbosely $ linkScript
    return ()


linkScript :: Sh ()
linkScript = do
    echo "----------------------------------------------"
    echo "Starting Linker Process"
    echo "----------------------------------------------"

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


