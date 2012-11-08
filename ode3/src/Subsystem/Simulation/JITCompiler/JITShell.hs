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
-- NOTE - needs to have a (VecMath-enabled) version of LLVM/Clang located on the PATH
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
    rm_f "./Sim.bc"
    rm_f "./Sim.ll"

    -- optimise the model
    when (L.get Sys.lOptimise p) $
        run "opt" (["-o", modelPath] ++ modelOpts ++ [modelPath]) >> return ()
    -- link the model to stdlib
    run "llvm-link" ["-o", simPath, modelPath, toTextIgnore libPath]
    -- opt the sim
--    when (L.get Sys.lOptimise p) $
--        run "opt" (["-o", simPath] ++ modelOpts ++ [simPath]) >> return ()
    -- perfrom LTO
    when (L.get Sys.lOptimise p) $
        run "opt" (["-o", simPath] ++ linkOpts ++ [simPath]) >> return ()

    -- DEBUG - dis-assemble sim.bc and gen graphs
    run "llvm-dis" [simPath]
    run "opt" ["-analyze", "-dot-callgraph", "-dot-cfg", "-dot-dom", simPath]

    return ()
  where
    -- TODO - fix these paths
    modelPath   = "./Model.bc"
    simPath     = "./Sim.bc"
    libDir      = "../res/StdLib"
    libPath     = libDir </> "OdeLibrary.bc" -- change to opt
    modelOpts   = if (L.get Sys.lOptimise p) then ["-std-compile-opts"] else []
    linkOpts     = if (L.get Sys.lOptimise p) then ["-std-link-opts", "-std-compile-opts"] else []

-- | Embedded script to executre a static simulation, utilising static linking to all libs
-- and no call-back to Ode run-time (requires use of Clang and system linker)
llvmAOTScript :: Sys.SimParams -> Sh ()
llvmAOTScript p = do
    liftIO $ debugM "ode3.sim" $ "Starting AOT Script"
    -- delete the old sim file
    rm_f output
    -- use clang to link our llvm-linked sim module to the system
    run "clang" $ [linkType, "-o", toTextIgnore output, optLevel, fastMath, simPath] ++ libDir ++ libs
    -- execute (as ext. process) if specified
    if (L.get Sys.lExecute p) then run output [] >> return () else return ()
    return ()
  where
    output      = "./Sim.exe"
    simPath     = "./Sim.bc"
    -- aotStubPath = "" -- "../res/StdLib/AOTStub.bc"
    libs        = ["-lm"]
    libDir      = []
    optLevel    = if (L.get Sys.lOptimise p) then "-O3" else "-O0"
    fastMath    = if (L.get Sys.lMathModel p /= Sys.StrictMath) then "-ffast-math" else ""
    -- check dyn/static linking
    linkType    = case (L.get Sys.lLinker p) of
        Sys.StaticLink -> "-static"
        Sys.DynamicLink -> ""
