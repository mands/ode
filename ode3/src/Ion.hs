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
import System.FilePath.Posix as SFP
import System.Log.Logger
import System.Log.Handler(close)
import System.Log.Handler.Simple

import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import AST.CoreFlat(SimType(..))

import Ion.Parser
import Ion.AST
import Ion.Process

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
            Right () -> return ()

    debugM "ion.main" $ "Quitting."
    -- TODO - return exit code depending on success/failure
    -- TODO - close filelogger


-- | start the source-source translation process
processFile :: FilePath -> MExceptIO ()
processFile inFile = do
    fileData <- liftIO $ readFile inFile
    liftIO $ putStrLn fileData

    -- parse, process and save the file
    ionAST <- mkExceptIO $ ionParse inFile fileData >>= processIon

    trace' [MkSB ionAST] "Final Ion AST" $ return ()

    -- save as Ode file

    liftIO $ mapM_ saveChannel ionAST
    liftIO $ saveOutFile

    return ()
  where
    saveChannel :: IonChannel -> IO ()
    saveChannel ionChan@IonChannel{..} = do
        putStrLn $ printf "\nStocimetric Matrix - (%d transitions x %d states)" (length transitions) (Set.size states)
        putStrLn $ printf "State row names - %s" (show states)
        putStrLn $ matrixToTable (fromJust stocMatrix)

        -- generate converted data
        case simType of
            SimODE -> genOde ionChan
            SimSDE -> genSde ionChan
            SimRRE -> genRre ionChan


    outFile = SFP.replaceExtension inFile "ode"

    saveOutFile = do
        TIO.writeFile outFile "Dummy\n"



genOde :: IonChannel -> IO ()
genOde ionChan@IonChannel{..} = do
    let detElems = getDetElems ionChan
    trace' [MkSB detElems] "Det elems" $ return ()
    return ()

genSde :: IonChannel -> IO ()
genSde ionChan@IonChannel{..} = do
    let detElems = getDetElems ionChan
    let stocElems = getStocElems ionChan
    trace' [MkSB detElems] "Det elems" $ return ()
    trace' [MkSB stocElems] "Stoc elems" $ return ()
    return ()

genRre :: IonChannel -> IO ()
genRre ionChan@IonChannel{..} = do
    undefined
