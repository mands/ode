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
import System.Environment(getArgs, getProgName)
import System.Console.GetOpt
import System.Directory(getCurrentDirectory)
import System.Log.Logger
import System.Log.Handler(close)
import System.Log.Handler.Simple

import Ion.Parser
import Ode.Parser
import Utilities

-- |main entry funtion
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
    let (flags, nonOpts, msgs) = getOpt RequireOrder options args
    print flags

    let fileName = head args

    -- read the input file
    infoM "ode3.main" $ "ode3 parsing " ++ fileName

    -- start the compiler
    output <- coreParser fileName

    -- output to screen and quit
    -- putStrLn output
    infoM "ode3.main" $ "Done"
    --close filelogger

-- |argument flag types
data Flag = Version
    deriving Show

-- |argument options
options :: [OptDescr Flag]
options = [ Option ['V'] ["version"] (NoArg Version) "Show version number" ]

-- |drives the ion language compilation state through monadic sequencing
ionParser :: FilePath -> IO ()
ionParser fileName = do
    fileData <- readFile fileName
    let parseRes = ionParse fileName fileData

    -- back in IO monad
    either (\err -> errorM "ode3.compilerDriver" err)
        (\res -> infoM "ode3.compilerDriver" "No errors" >> print res) parseRes

-- |drives the core language compilation state through monadic sequencing
coreParser :: FilePath -> IO ()
coreParser fileName = do
    fileData <- readFile fileName
    let parseRes = odeParse fileName fileData

    -- back in IO monad
    either (\err -> errorM "ode3.compilerDriver" err)
        (\res -> infoM "ode3.compilerDriver" "No errors" >> print res) parseRes

