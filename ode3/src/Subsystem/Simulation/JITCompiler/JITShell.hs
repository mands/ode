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
optScript, linkStdlibScript, compileScript, executeSimScript
) where

-- Labels
import Control.Category
import qualified Data.Label as L
import Prelude hiding ((.), id)

import qualified System.FilePath as FP
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
projRootPath    = "/home/mandeep/DPhil/Projects/root"
projLibPath     = projRootPath </> "lib"
odeRootPath = "../res/StdLib"
odeLibPath  = odeRootPath </> "lib"

modelBC     = "./.Model.bc"
modelOptBC  = "./.Model.opt.bc"
simBC       = "./.Sim.bc"
exeOutput   = "./.Sim.exe"
odeObjFile  = "./.OdeModel.o"

optScript :: Sys.SimParams -> Sh ()
optScript p@(Sys.SimParams{..}) = do
    liftIO $ debugM "ode3.sim" $ "Starting LLVM Opt Script"
    -- delete the old opt file?

    -- optimise the model
    modelBC' <- if (_optimise)
        -- if
        then if (_mathModel == Sys.Fast)
            then if (_vecMath)
                -- fastmath & vecmath then run full vecmath opts
                then do
                    -- 1st pass - std-compile-opts, liftvecmath, bb-vectorise1 (pick up all std chain#s (length 4+))
                    -- disabled as of llvm 3.2 - wait for improved cost-analysis mechanism in later llvm releases
--                    let modelVec1 = "./Model.vec1.bc"
--                    run_ "opt" $ ["-load", llvmVecMath, "-o", modelVec1 ] ++
--                        [ "-std-compile-opts", "-liftvecmath", "-bb-vectorize"
--                        -- main params
--                        , "-bb-vectorize-vecmath-pass=0", "-bb-vectorize-req-chain-depth=6", "-bb-vectorize-use-chain-depth=1", "-bb-vectorize-aligned-only=1", "-bb-vectorize-splat-breaks-chain=1"
--                        -- no built-in or vecmath intrinsics
--                        , "-bb-vectorize-no-math=1", "-bb-vectorize-no-vecmath=1"
--                        -- disable all other instructinos except FP ops
--                        , "-bb-vectorize-no-bools=1", "-bb-vectorize-no-ints=1", "-bb-vectorize-no-pointers=1", "-bb-vectorize-no-casts=1", "-bb-vectorize-no-fma=1"
--                        , "-bb-vectorize-no-select=1", "-bb-vectorize-no-cmp=1", "-bb-vectorize-no-gep=1", "-bb-vectorize-no-mem-ops=1"
--                        -- debug output
--                        ,"-bb-vectorize-debug-instruction-examination=0", "-bb-vectorize-debug-candidate-selection=0", "-bb-vectorize-debug-pair-selection=0"
--                        ,"-stats", modelBC]
--                    run_ "llvm-dis" [modelVec1]

                    -- dummy 1st pass - std-compile-opts, liftvecmath
                    let modelVec1 = "./.Model.vec1.bc"
                    run_ "opt" $ ["-load", llvmVecMath, "-o", modelVec1 ] ++ [ "-std-compile-opts", "-liftvecmath", "-stats", modelBC]
                    run_ "llvm-dis" [modelVec1]
                    -- 2nd pass - bb-vectorise2 (pick up all short vecmath-only chains (length 1-2)), lowervecmath, std-compile-opts
                    let modelVec2 = "./.Model.vec2.bc"
                    run_ "opt" $ ["-load", llvmVecMath, "-o", modelVec2 ] ++
                        ["-bb-vectorize", "-lowervecmath", "-std-compile-opts"
                        -- main params
                        ,"-bb-vectorize-vecmath-pass=1", "-bb-vectorize-req-chain-depth=2", "-bb-vectorize-use-chain-depth=1", "-bb-vectorize-aligned-only=1", "-bb-vectorize-splat-breaks-chain=1"
                        -- debug output
                        -- ,"-bb-vectorize-debug-instruction-examination=0", "-bb-vectorize-debug-candidate-selection=1", "-bb-vectorize-debug-pair-selection=0"
                        -- vector length (TODO - make a sim param) (NOTE - broken atm, SSE->AVX has probs)
                        -- ,"-bb-vectorize-vector-bits=256"
                        ,"-stats", modelVec1]
                    run_ "llvm-dis" [modelVec2]
                    linkVecMath modelVec2

                -- just fastmath, run lift and lowering (as implies linking to finite-funcs)
                else do
                    let modelLift = "./.Model.lift.bc"
                    run_ "opt" (["-load", llvmVecMath, "-o", modelLift ] ++
                        ["-liftvecmath", "-lowervecmath", "-std-compile-opts", "-stats", modelBC])
                    run_ "llvm-dis" [modelLift]
                    linkVecMath modelLift

            -- no fastmath - just optimise
            else run_ "opt" (["-o", modelBC] ++ modelOpts ++ [modelBC]) >> return modelBC
        else return modelBC

    -- copy the opt file to final location
    cp (fromText modelBC') (fromText modelOptBC)
    run_ "llvm-dis" [modelBC]
    run_ "llvm-dis" [modelOptBC]
    return ()
  where
    llvmVecMath = toTextIgnore $ projLibPath </> "LLVMVecMath.so"
    modelOpts   = ["-std-compile-opts"]

    -- link the model to the vecmath implementation, and perfomrm LTO/opts
    linkVecMath modelBC' = do
        -- link the model to stdlib (& vecmath)
        run_ "llvm-link" $ ["-o", modelVecBC, modelBC', odeVecMathLib]
        run_ "opt" $ ["-o", modelVecBC] ++ linkOpts ++ [modelVecBC]
        run_ "llvm-dis" [modelVecBC]
        return modelVecBC
      where
        modelVecBC  = "./.Model.vecmath.bc"
        -- we can't use link-opts here, as ends up removing everything (assumes full program)
        linkOpts    = ["-std-compile-opts"] -- ["-std-link-opts", "-std-compile-opts"]
        -- need to switch depending on the mathmodel
        odeVecMathLib = toTextIgnore $ odeLibPath </> case _mathLib of
                                                        Sys.GNU     -> "VecMath_GNU.bc"
                                                        Sys.AMD     -> "VecMath_AMD.bc"
                                                        Sys.Intel   -> "VecMath_Intel.bc"


-- | Embedded script to link to the Ode stdlib
linkStdlibScript :: Sys.SimParams -> Sh ()
linkStdlibScript p@(Sys.SimParams{..}) = do
    liftIO $ debugM "ode3.sim" $ "Starting LLVM Linker Script"
    -- delete the old sim file
    rm_f ".Sim.bc"
    rm_f ".Sim.ll"
    -- rm_f "./*.dot"
    -- link the model to stdlib and cvodeSim
    if (_odeSolver == Sys.Adaptive)
        then run_ "llvm-link" ["-o", simBC, modelOptBC, odeStdLib, odeCvodeSim]
        else run_ "llvm-link" ["-o", simBC, modelOptBC, odeStdLib]
    -- perform LTO (only for AOT atm, need to investigate JIT support)
    when (L.get Sys.lOptimise p && L.get Sys.lBackend p == Sys.AOTCompiler) $ do
        run_ "llvm-dis" ["-o", simLtoBC, simBC]
        run_ "opt" (["-o", simBC] ++ linkOpts ++ [simBC])

    -- DEBUG - dis-assemble sim.bc and gen graphs
    run_ "llvm-dis" [simBC]
    -- run_ "opt" ["-analyze", "-dot-callgraph", "-dot-cfg", "-dot-dom", simBC]
    return ()
  where
    odeStdLib   = toTextIgnore $ odeLibPath </> "libOde.bc" -- TODO - change to opt stdlib?
    odeCvodeSim = toTextIgnore $ odeLibPath </> "CvodeSim.bc" -- TODO - change to opt stdlib?
    linkOpts    = ["-std-link-opts", "-std-compile-opts"]
    simLtoBC    = "./.Sim.pre-lto.ll"
    simLtoBC'    = "./.Sim.pre-lto.bc"


-- | Embedded script to compile a native AOT represetnation of the model (& optional simulation)
-- with no call-back to Ode run-time (requires use of Clang and system linker)
compileScript :: Sys.SimParams -> Sh ()
compileScript p@(Sys.SimParams{..}) = do
    liftIO $ debugM "ode3.sim" $ "Starting AOT/Compile Script"
    -- delete the old sim file
    -- rm_f exeOutput
    -- rm_f odeObjFile

    if (L.get Sys.lBackend p == Sys.ObjectFile)
        -- use clang to create a object file
        then do
                run "clang" $ ["-march=x86-64", "-integrated-as", "-c", "-o", toTextIgnore odeObjFile, optLevel]
                    ++ maybeToList fastMath ++ [modelOptBC]
                cp odeObjFile (fromText . LT.pack $ FP.addExtension outName "o")
        -- use clang to link our llvm-linked sim module to the system
        else do
            run "clang" $ (maybeToList linkType) ++ ["-march=x86-64", "-integrated-as", "-o", toTextIgnore exeOutput, optLevel]
                ++ maybeToList fastMath ++ [simBC] ++ ["-L", toTextIgnore projLibPath] ++ cvodeLibs ++ mathLibs
            cp exeOutput (fromText . LT.pack $ FP.addExtension outName "exe")
    return ()
  where
    -- aotStubPath = "" -- "../res/StdLib/AOTStub.bc"

    -- need to switch depending on the mathmodel
    mathLibs    = if _optimise && (L.get Sys.lMathModel p == Sys.Fast)
                    then case _mathLib of
                        Sys.GNU     -> ["-lm"]
                        Sys.AMD     -> ["-lamdlibm", "-lm"]
                        Sys.Intel   -> ["-lsvml", "-limf", "-lirc", "-lm"]
                    else ["-lm"]

    cvodeLibs   = if (_odeSolver == Sys.Adaptive)
                    then ["-lsundials_cvode", "-lsundials_nvecserial"]
                    else []

    -- odeVecMathLib = toTextIgnore $ odeLibPath </> odeVecMathFile
    optLevel    = if (L.get Sys.lOptimise p) then "-O3" else "-O0"
    fastMath    = if _optimise && (L.get Sys.lMathModel p == Sys.Fast) then Just "-ffast-math" else Nothing
    -- check dyn/static linking
    linkType    = case (L.get Sys.lLinker p) of
        Sys.Static -> Just "-static"
        Sys.Dynamic -> Nothing

    outName     = FP.dropExtension $ L.get Sys.lExeName p

-- | Executes simulation of standalone executable in sub-process (any required libs must be on LD_LIBRARY_PATH)
executeSimScript ::  Sys.SimParams -> Sh ()
executeSimScript p@(Sys.SimParams{..}) = do
    -- execute (as ext. process) if specified
    when (L.get Sys.lExecute p) $ run_ exeOutput []

