-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))

--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- Main entry file for ode3 ion channel front-end
--
-----------------------------------------------------------------------------

import System.IO(stdout)
import System.IO as SIO
import System.Environment(getArgs, getProgName)
import System.Directory(getCurrentDirectory)
import System.Posix.Files as PF
import System.Log.Logger
import System.Log.Handler(close)
import System.Log.Handler.Simple

import Utils.Utils
import Utils.OrdMap
import ShellUI

-- | main entry funtion
main :: IO ()
main = do
    -- TODO - better args handling needed

    -- setup the logger
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    --filelogger <- fileHandler "output.log" DEBUG
    streamlogger <- verboseStreamHandler stdout DEBUG
    --updateGlobalLogger rootLoggerName (setHandlers [streamlogger, filelogger])
    updateGlobalLogger rootLoggerName (setHandlers [streamlogger])

    -- some debug stuffOd
    progName <- getProgName
    debugM "ode3.main" $ "Hello from " ++ progName ++ "!"
    curDir <- getCurrentDirectory
    debugM "ode3.main" $ "Running from " ++ curDir

    -- start the console interface to the compiler
    shellEntry

    debugM "ode3.main" $ "Quitting."

    -- TODO - return exit code depending on success/failure
    -- TODO - close filelogger

-- | driver for the middle-end of the compiler
-- will run the middle-end of the compiler using the ANF/lowerlevel FIR - AST to be determined
-- basically runs a sequence of optimisations over the model, will some checking/valdiation/simulation
-- to check optimisation results
coreANFDriver = undefined

-- | driver for the back-end of the compiler
 -- takes a final optimised model in the low-level AST, and runs the code-generation backend, either through
-- an interpreter, LLVM CPU, or OpenCL
codeGenDriver = undefined
