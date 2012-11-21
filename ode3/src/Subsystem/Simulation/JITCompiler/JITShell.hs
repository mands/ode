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

import Data.Maybe (catMaybes, maybeToList)
import Utils.CommonImports
import Subsystem.Simulation.Common
import Subsystem.Simulation.JITCompiler.JITCommon
import Subsystem.Simulation.JITCompiler.JITModel
import qualified Subsystem.SysState as Sys

default (LT.Text)

-- Global Constants
-- TODO - fix these hardcoded paths
rootPath    = "/home/mandeep/DPhil/Projects/root"
libPath     = rootPath </> "lib"
modelBC     = "./Model.bc"
simBC       = "./Sim.bc"
exeOutput   = "./Sim.exe"
odeLibPath  = "../res/StdLib"


-- | Embedded script to optimise the model, and link to the simulation library
llvmLinkScript :: Sys.SimParams -> Sh ()
llvmLinkScript p@(Sys.SimParams{..}) = do
    liftIO $ debugM "ode3.sim" $ "Starting LLVM Linker Script"
    -- delete the old sim file
    rm_f "Sim.bc"
    rm_f "Sim.ll"
    -- rm_f "./*.dot"

    -- optimise the model
    modelBC' <- if (_optimise)
        -- if
        then if (_mathModel == Sys.Fast)
            then if (_vecMath)
                -- fastmath & vecmath then run full vecmath opts
                then do
                    -- 1st pass - std-compile-opts, liftvecmath, bb-vectorise1 (pick up all std chains (length 4+))
                    -- TODO - we don't run this pass yet until LLVM 3.2 released
--                    run_ "opt" $ ["-load", llvmVecMath, "-o", LT.append modelBC "vec2" ] ++
--                        ["-liftvecmath", "-std-compile-opts", "-bb-vectorize", "-lowervecmath" -- lift and lower
--                        ,"-bb-vectorize-vecmath-pass=0", "-bb-vectorize-req-chain-depth=7", "-bb-vectorize-pow2-len-only=0"
--                        ,"-bb-vectorize-no-floats=0", "-bb-vectorize-no-math=1", "-bb-vectorize-no-vecmath=1", "-bb-vectorize-no-fma=0"
--                        ,"-bb-vectorize-aligned-only=1", "-bb-vectorize-no-mem-op-boost=0"
--                        ,"-bb-vectorize-debug-instruction-examination=0", "-bb-vectorize-debug-candidate-selection=0", "-bb-vectorize-debug-pair-selection=0", "-bb-vectorize-debug-cycle-check=0"
--                        ,"-stats", modelBC]
                    -- 2nd pass - bb-vectorise2 (pick up all short vecmath-only chains (length 1-2)), lowervecmath, std-compile-opts
                    let modelVec2 = "./Model.vec2.bc"
                    run_ "opt" $ ["-load", llvmVecMath, "-o", modelVec2 ] ++
                        ["-liftvecmath", "-bb-vectorize", "-lowervecmath", "-std-compile-opts"
                        ,"-bb-vectorize-vecmath-pass=1", "-bb-vectorize-req-chain-depth=2", "-bb-vectorize-pow2-len-only=1"
                        ,"-bb-vectorize-no-floats=1", "-bb-vectorize-no-math=1", "-bb-vectorize-no-vecmath=0", "-bb-vectorize-no-fma=0"
                        ,"-bb-vectorize-aligned-only=1", "-bb-vectorize-no-mem-op-boost=0"
                        ,"-bb-vectorize-debug-instruction-examination=0", "-bb-vectorize-debug-candidate-selection=1", "-bb-vectorize-debug-pair-selection=0", "-bb-vectorize-debug-cycle-check=0"
                        ,"-stats", modelBC ] --LT.append modelBC "vec1"]
                    run_ "llvm-dis" [modelVec2]
                    return modelVec2
                -- just fastmath, run lift and lowering (as implies linking to finite-funcs)
                else do
                    let modelLift = "./Model.lift.bc"
                    run_ "opt" (["-load", llvmVecMath, "-o", modelLift ] ++
                        ["-liftvecmath", "-lowervecmath", "-std-compile-opts", "-stats", modelBC])
                    run_ "llvm-dis" [modelLift]
                    return modelLift

            -- no fastmath - just optimise
            else run_ "opt" (["-o", modelBC] ++ modelOpts ++ [modelBC]) >> return modelBC
        else return modelBC

    -- link the model to stdlib (& vecmath)
    run_ "llvm-link" $ ["-o", simBC] ++ [modelBC', odeStdLib] ++ (maybeToList odeVecMathLib)

    -- perform LTO
    when (L.get Sys.lOptimise p) $
        run_ "opt" (["-o", simBC] ++ linkOpts ++ [simBC])

    -- DEBUG - dis-assemble sim.bc and gen graphs
    run_ "llvm-dis" [simBC]
    -- run_ "opt" ["-analyze", "-dot-callgraph", "-dot-cfg", "-dot-dom", simBC]

    return ()
  where
    odeStdLib  = toTextIgnore $ odeLibPath </> "OdeLibrary.bc" -- change to opt stdlib
    modelOpts   = ["-std-compile-opts"]
    linkOpts    = ["-std-link-opts", "-std-compile-opts"]
    llvmVecMath = toTextIgnore $ libPath </> "LLVMVecMath.so"

    -- need to switch depending on the mathmodel
    odeVecMathLib = if _optimise && (L.get Sys.lMathModel p == Sys.Fast)
                                        then Just . toTextIgnore $ odeLibPath </> case _mathLib of
                                            Sys.GNU     -> "VecMath_GNU.bc"
                                            Sys.AMD     -> "VecMath_AMD.bc"
                                            Sys.Intel   -> "VecMath_Intel.bc"
                                        else Nothing

-- | Embedded script to executre a static simulation, utilising static linking to all libs
-- and no call-back to Ode run-time (requires use of Clang and system linker)
llvmAOTScript :: Sys.SimParams -> Sh ()
llvmAOTScript p@(Sys.SimParams{..}) = do
    liftIO $ debugM "ode3.sim" $ "Starting AOT Script"
    -- delete the old sim file
    rm_f exeOutput

    -- use clang to link our llvm-linked sim module to the system
    run "clang" $ (maybeToList linkType) ++ ["-integrated-as", "-o", toTextIgnore exeOutput, optLevel] ++ maybeToList fastMath ++ [simBC]
        ++ ["-L", toTextIgnore libPath] ++ libs

    -- execute (as ext. process) if specified
    when (L.get Sys.lExecute p) $ run_ exeOutput []

    return ()
  where
    -- aotStubPath = "" -- "../res/StdLib/AOTStub.bc"

    -- need to switch depending on the mathmodel
    libs  = if _optimise && (L.get Sys.lMathModel p == Sys.Fast)
                then case _mathLib of
                    Sys.GNU     -> ["-lm", crtFastMath]
                    Sys.AMD     -> ["-lamdlibm", "-lm", crtFastMath]
                    Sys.Intel   -> ["-lsvml", "-limf", "-lirc", "-lm", crtFastMath]
                else ["-lm"]

    -- odeVecMathLib = toTextIgnore $ odeLibPath </> odeVecMathFile
    crtFastMath = "-Wl,--no-as-needed,../res/StdLib/crtfastmath.o"

    optLevel    = if (L.get Sys.lOptimise p) then "-O3" else "-O0"
    fastMath    = if _optimise &&  (L.get Sys.lMathModel p == Sys.Fast) then Just "-ffast-math" else Nothing
    -- check dyn/static linking
    linkType    = case (L.get Sys.lLinker p) of
        Sys.Static -> Just "-static"
        Sys.Dynamic -> Nothing
