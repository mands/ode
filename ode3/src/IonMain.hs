-----------------------------------------------------------------------------
--
-- Module      :  IonMain
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- Stuff
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

-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO

import Parser.Ion
import Utils.CommonImports

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
    debugM "ion.main" $ "Hello from " ++ progName ++ "!"
    curDir <- getCurrentDirectory
    debugM "ion.main" $ "Running from " ++ curDir


    -- get the file from the args#
    argsLen <- length <$> getArgs
    if argsLen /= 1
      then do
        putStrLn "Incorrect command-line args - usage : ion INFILE"
      else do
        inFile <- head <$> getArgs
        debugM "ion.main" $ printf "Input file : %s" (show inFile)
        res <- runErrorT $ processFile inFile
        case res of
            Left err -> putStrLn $ "Error - " ++ err
            Right () -> return ()

    debugM "ion.main" $ "Quitting."
    -- TODO - return exit code depending on success/failure
    -- TODO - close filelogger


-- | start the source-source translation process
processFile :: FilePath -> MExceptIO ()
processFile inFile = do
    fileData <- liftIO $ readFile inFile
    liftIO $ putStrLn fileData

    -- parse the file
    ionAST <- mkExceptIO $ ionParse inFile fileData

    trace' [MkSB ionAST] "Ion AST" $ return ()
    -- process the file

    -- save as Ode file

    return ()
