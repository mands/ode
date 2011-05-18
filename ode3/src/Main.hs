-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
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
import System.Environment(getArgs, getProgName)
import System.Directory(getCurrentDirectory)
import System.Log.Logger
import System.Log.Handler(close)
import System.Log.Handler.Simple

import Ion.Parser
import Core.Parser
import Utilities

main :: IO ()
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    --filelogger <- fileHandler "output.log" DEBUG
    streamlogger <- verboseStreamHandler stdout INFO
    --updateGlobalLogger rootLoggerName (setHandlers [streamlogger, filelogger])
    updateGlobalLogger rootLoggerName (setHandlers [streamlogger])

    progName <- getProgName

    infoM "ode3.main" $ "Hello World from " ++ progName ++ "!"

    curDir <- getCurrentDirectory
    infoM "ode3.main" $ "Running from " ++ curDir

    -- get the input filename, better args handling needed
    args <- getArgs
    let fileName = head args

    -- read the input file
    infoM "ode3.main" $ "ode3 parsing " ++ fileName

    -- start the compiler
    output <- coreParser fileName

    -- output to screen and quit
    -- putStrLn output
    infoM "ode3.main" $ "Done"
    --close filelogger

-- | drives the compilation stae through monadic sequencing
ionParser :: FilePath -> IO ()
ionParser fileName = do
    fileData <- readFile fileName
    let parseRes = ionParse fileName fileData

    -- back in IO monad
    either (\err -> errorM "ode3.compilerDriver" err)
        (\res -> infoM "ode3.compilerDriver" "No errors" >> print res) parseRes

coreParser :: FilePath -> IO ()
coreParser fileName = do
    fileData <- readFile fileName
    let parseRes = coreParse fileName fileData

    -- back in IO monad
    either (\err -> errorM "ode3.compilerDriver" err)
        (\res -> infoM "ode3.compilerDriver" "No errors" >> print res) parseRes

