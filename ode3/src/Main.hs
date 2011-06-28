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
import qualified Ode.AST as O
import qualified Core.AST as C
import qualified CoreANF.AST as CA
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
    output <- odeParser fileName

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
    either (\err -> errorM "ode3.ionParser" err)
        (\res -> infoM "ode3.ionParser" "No errors" >> print res) parseRes

-- |drives the core language compilation state through monadic sequencing
odeParser :: FilePath -> IO ()
odeParser fileName = do
    fileData <- readFile fileName
    let parseRes =

    let res = (odeParse fileName fileData) >>=

    -- back in IO monad
    either (\err -> errorM "ode3.odeParser" err)
        (\res -> infoM "ode3.odeParser" "No errors" >> print res) parseRes


-- |driver for the core language front-end of the compiler
-- will effectively run the front-end pipeline within the Error monad
-- requires calling reorderer, renamer, typechecker, converter/interpreter
coreDriver :: C.Model -> CA.Model
coreDriver oModel = undefined

-- |driver for the middle-end of the compiler
-- will run the middle-end of the compiler using the ANF/lowerlevel FIR - AST to be determined
-- basically runs a sequence of optimisations over the model, will some checking/valdiation/simulation
-- to check optimisation results
coreANFDriver = undefined

-- |driver for the back-end of the compiler
-- takes a final optimised model in the low-level AST, and runs the code-generation backend, either through
-- an interpreter, LLVM CPU, or OpenCL
codeGenDriver = undefined
