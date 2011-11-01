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
import Control.Monad.Trans
import Control.Monad
import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Data.List as List

import Core.ModuleParser (modParse)
import qualified Core.ExprAST as EA
import qualified Core.ModuleAST as MA
--import qualified CoreANF.AST as CA

import Utils.Utils
import Utils.OrdMap

-- | main entry funtion
main :: IO ()
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    --filelogger <- fileHandler "output.log" DEBUG
    streamlogger <- verboseStreamHandler stdout DEBUG
    --updateGlobalLogger rootLoggerName (setHandlers [streamlogger, filelogger])
    updateGlobalLogger rootLoggerName (setHandlers [streamlogger])

    progName <- getProgName

    infoM "ode3.main" $ "Hello World from " ++ progName ++ "!"

    curDir <- getCurrentDirectory
    infoM "ode3.main" $ "Running from " ++ curDir

    -- get the input filename, better args handling needed
    args <- getArgs
    let (flags, nonOpts, msgs) = getOpt RequireOrder options args
    -- print flags

    let fileName = head args

    -- read the input file
    infoM "ode3.main" $ "parsing " ++ fileName

    -- start the compiler
    --(odeParser fileName) >>= coreDriver
    -- need to create a maybeT transformer - ignore for now
    -- TODO - return exit code depending on success/failure
    resA <- modParser fileName
    -- maybe (return Nothing) coreDriver resA

    infoM "ode3.main" $ "Done"
    --close filelogger


-- | debugging main entry funtion that takes the input file as a func param
debugMain :: String -> IO ()
debugMain fileName = do
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
    resA <- modParser fileName
    -- maybe (return Nothing) coreDriver resA

    infoM "ode3.main" $ "Done"
    --close filelogger


-- | argument flag types
data Flag = Version
    deriving Show

-- | argument options
options :: [OptDescr Flag]
options = [ Option ['V'] ["version"] (NoArg Version) "Show version number" ]

modParser :: FilePath -> IO ()
modParser fileName = do
    -- TODO - update to follow multiple file commands, move reader into the parser
    fileData <- readFile fileName
    let modEnv = modParse fileName fileData Map.empty
    either (\err -> errorM "ode3.modParser" err)
        (\res -> infoM "ode3.modParser" $ "Parsed modules - \n" ++ (show res)) modEnv


-- | drives the ion language compilation state through monadic sequencing
--ionParser :: FilePath -> IO ()
--ionParser fileName = do
--    fileData <- readFile fileName
--    let res = ionParse fileName fileData
--
--    either (\err -> errorM "ode3.ionParser" err)
--        (\res -> infoM "ode3.ionParser" "No errors" >> print res) res

-- | drives the core language compilation state through monadic sequencing
--odeParser :: FilePath -> IO (Maybe [C.TopMod C.SrcId])
--odeParser fileName = do
--    -- TODO - update to follow multiple file commands
--    fileData <- readFile fileName
--    let modules = (modParse fileName fileData) >>= desugar
--
--    -- back in IO monad, report any error message(s)
--    either
--        (\err -> errorM "ode3.odeParser" err >> return Nothing)
--        (\res -> infoM "ode3.odeParser" "No errors"
--                >> debugM "ode3.odeParser" (succOut res)
--                >> return (Just res))
--        modules
--
--  where
--    succOut srcMods = "Parsed Modules - \n" ++ (List.intercalate "\n" . List.map show $ srcMods)

--coreDriver :: [C.TopMod C.SrcId] -> IO (Maybe C.ModuleEnv)
--coreDriver modules = MD.moduleDriver modules

-- | driver for the middle-end of the compiler
-- will run the middle-end of the compiler using the ANF/lowerlevel FIR - AST to be determined
-- basically runs a sequence of optimisations over the model, will some checking/valdiation/simulation
-- to check optimisation results
coreANFDriver = undefined

-- | driver for the back-end of the compiler
-- takes a final optimised model in the low-level AST, and runs the code-generation backend, either through
-- an interpreter, LLVM CPU, or OpenCL
codeGenDriver = undefined
