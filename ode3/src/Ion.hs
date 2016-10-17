-----------------------------------------------------------------------------
--
-- Module      :  IonMain
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  BSD3
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
-- import System.Posix.Files as PF
import System.FilePath as SF
import System.Log.Logger
import System.Log.Handler(close)
import System.Log.Handler.Simple

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import AST.CoreFlat(SimType(..))

import Ion.Parser
import Ion.CodeGen
import Ion.AST
import Ion.Process

import Ode.Utils.CommonImports

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


    -- get the file from the args
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
            Right () -> debugM "ion.main" "All sucessful." >> return ()

    debugM "ion.main" $ "Quitting."
    -- TODO - return exit code depending on success/failure
    -- TODO - close filelogger


-- | start the source-source translation process
processFile :: FilePath -> MExceptIO ()
processFile inFile = do
    fileData <- liftIO $ readFile inFile
    liftIO $ putStrLn fileData
    -- parse, process and save the file
    ionOut  <- mkExceptIO $ ionParse inFile fileData >>= processIon >>= ionCodeGen
    liftIO $ writeFile outFile ionOut
    liftIO $ putStrLn ionOut


    -- all done!
    return ()
  where
    outFile = SF.replaceExtension inFile "od3"

-- TODO - integrate this correctly, use box output
dumpData :: IonChannel -> IO ()
dumpData ionChan@IonChannel{..} = do
    putStrLn $ printf "\nStocimetric Matrix - (%d transitions x %d states)" (length transitions) (Set.size states)
    putStrLn $ printf "State row names - %s" (show states)
    putStrLn $ matrixToTable (fromJust stocMatrix)

