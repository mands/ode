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
-- Main ODEc entry file, takes a source file on the cmd line and either
-- interprets or compiles to native / OpenCL code
--
-----------------------------------------------------------------------------

import System.IO(stdout)
import System.Environment(getArgs, getProgName)
import System.Directory(getCurrentDirectory)
import System.Log.Logger
import System.Log.Handler(close)
import System.Log.Handler.Simple

import Data.Char(toUpper)
import qualified Data.Map as Map

import Utilities
import Parser
import qualified ModelSemantics as M
import AST
--import qualified ModelAST as M
import qualified ExecutableAST as E
--import qualified TypedAST as T
import qualified UInterpret as I
import qualified Simulate as S
--import qualified Interpreter as I
--import qualified Scratch as S
--import qualified Compile as C
--import qualified CodeGen as C

-- | main entry point
main :: IO ()
main = do
    updateGlobalLogger rootLoggerName (setLevel DEBUG)
    --filelogger <- fileHandler "output.log" DEBUG
    streamlogger <- verboseStreamHandler stdout INFO
    --updateGlobalLogger rootLoggerName (setHandlers [streamlogger, filelogger])
    updateGlobalLogger rootLoggerName (setHandlers [streamlogger])

    progName <- getProgName

    infoM "ode2.main" $ "Hello World from " ++ progName ++ "!"


    curDir <- getCurrentDirectory
    putStrLn ("Running from " ++ curDir)


    -- get the input filename, better args handling needed
    args <- getArgs
    let fileName = head args

    -- read the input file
    infoM "ode2.main" $ "ode2 compiling " ++ fileName

    -- start the compiler
    output <- compilerDriver fileName

    -- output to screen and quit
    -- putStrLn output
    infoM "ode2.main" $ "Done"
    --close filelogger

-- | drives the compilation stae through monadic sequencing
compilerDriver :: FilePath -> IO ()
compilerDriver fileName = do
    fileData <- readFile fileName

    -- enter the error monad to the rescue
    -- parse files into model ast, check/optimise model ast, convert to \ -calc
    -- type exe ast and send to haskell interpreter
    -- let final :: (T.Type a) => MExcept [T.TFun a]
    --let final :: MExcept [T.TFun]
--    let final = (odeParse fileName fileData) >>= M.modelCheck >>=  E.convertAST >>= I.startSim
    let final = (odeParse fileName fileData) >>=  M.modelCheck >>= E.convertAST >>= S.startSim

        -- >>= T.typeCheck' -- >>= C.codegen
        -- printModel parseRes
        -- printExecutable res
    -- back in IO monad

    either (\err -> errorM "ode2.compilerDriver" err)
        (\res -> infoM "ode2.compilerDriver" "No errors" >> res) final -- >> mapM_ print res) (final :: (T.Type a) => MExcept [T.TFun a])

    --print S.runner
    --print S.funrunner
    --print "test"

{-
test :: (Translate a) => [T.TFun a] -> IO ()
test = codegen
-}

