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
import Control.Monad.Trans
import Control.Monad
import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Data.List as List

import Core.ModuleParser (modParse)
import qualified Core.ExprAST as EA
import qualified Core.ModuleAST as MA

import Utils.Utils
import Utils.OrdMap

debugPipe :: FilePath
debugPipe = "./.odepipe"

-- | main entry funtion
main :: IO ()
main = do
    -- TODO - better args handling needed
    -- get the input filename
    fileName <- liftM head getArgs
    compilerStart fileName

compilerStart :: String -> IO ()
compilerStart fileName = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    --filelogger <- fileHandler "output.log" DEBUG
    streamlogger <- verboseStreamHandler stdout DEBUG
    --updateGlobalLogger rootLoggerName (setHandlers [streamlogger, filelogger])
    updateGlobalLogger rootLoggerName (setHandlers [streamlogger])

    progName <- getProgName

    infoM "ode3.main" $ "Hello World from " ++ progName ++ "!"

    curDir <- getCurrentDirectory
    infoM "ode3.main" $ "Running from " ++ curDir

    -- read the input file
    infoM "ode3.main" $ "parsing " ++ fileName


    -- open up the named pipe
    pExists <- (PF.fileExist debugPipe)
    pStatus <- (PF.getFileStatus debugPipe)
    infoM "ode3.main" $ "Exists - " ++ show pExists
    infoM "ode3.main" $ "Named Pipe - " ++ show (PF.isNamedPipe pStatus)
    hCmdPipe <- SIO.openFile debugPipe SIO.ReadMode
    SIO.hSetBuffering hCmdPipe SIO.LineBuffering

    -- read lines from the pipe until EOF
    hshow <- SIO.hShow hCmdPipe
    infoM "ode3.main" $ "Handle - " ++ hshow

    --b <- SIO.hWaitForInput hCmdPipe (-1)
    --infoM "ode3.main" $ "Wait - " ++ show b
    readLoop hCmdPipe

    -- start the compiler
    -- resA <- modParser fileName
    -- TODO need to create a maybeT transformer - ignore for now

    infoM "ode3.main" $ "Done"

    -- Clean up
    SIO.hClose hCmdPipe

    -- TODO - return exit code depending on success/failure
    -- TODO - close filelogger


readLoop :: Handle -> IO ()
readLoop hCmdPipe = forever (SIO.hGetLine hCmdPipe >>= (\s -> infoM "ode3.main" $ "Read - " ++ s))


modParser :: FilePath -> IO ()
modParser fileName = do
    fileData <- readFile fileName
    let modEnv = modParse fileName fileData Map.empty
    either (\err -> errorM "ode3.modParser" err)
        (\res -> infoM "ode3.modParser" $ "Parsed modules - \n" ++ (show res)) modEnv


-- | driver for the middle-end of the compiler
-- will run the middle-end of the compiler using the ANF/lowerlevel FIR - AST to be determined
-- basically runs a sequence of optimisations over the model, will some checking/valdiation/simulation
-- to check optimisation results
coreANFDriver = undefined

-- | driver for the back-end of the compiler
 -- takes a final optimised model in the low-level AST, and runs the code-generation backend, either through
-- an interpreter, LLVM CPU, or OpenCL
codeGenDriver = undefined
