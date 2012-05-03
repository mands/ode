-----------------------------------------------------------------------------
--
-- Module      :  ShellHandle
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | A modification of Shellac basic backend to take an input handle parameter on init
--
-----------------------------------------------------------------------------

module ShellHandle (
basicBackend
) where

import System.IO   ( stdout, stderr, stdin, hFlush, hPutStr, hPutStrLn
                   , hGetLine, hGetChar, hGetBuffering, hSetBuffering, Handle
                   , BufferMode(..)
                   )
import qualified Control.Exception as Ex

import System.Console.Shell.Backend

basicBackend :: Handle -> ShellBackend ()
basicBackend hnd = ShBackend
  { initBackend                      = return ()
  , shutdownBackend                  = \_ -> return ()
  , outputString                     = \_ -> basicOutput
  , flushOutput                      = \_ -> hFlush stdout
  , getSingleChar                    = \_ -> basicGetSingleChar hnd
  , getInput                         = \_ -> basicGetInput hnd
  , addHistory                       = \_ _ -> return ()
  , setWordBreakChars                = \_ _ -> return ()
  , getWordBreakChars                = \_ -> return defaultWordBreakChars
  , onCancel                         = \_ -> hPutStrLn stdout "canceled...\n"
  , setAttemptedCompletionFunction   = \_ _ -> return ()
  , setDefaultCompletionFunction     = \_ _ -> return ()
  , completeFilename                 = \_ _ -> return []
  , completeUsername                 = \_ _ -> return []
  , clearHistoryState                = \_ -> return ()
  , getMaxHistoryEntries             = \_ -> return 0
  , setMaxHistoryEntries             = \_ _ -> return ()
  , readHistory                      = \_ _ -> return ()
  , writeHistory                     = \_ _ -> return ()
  }

basicGetSingleChar :: Handle -> String -> IO (Maybe Char)
basicGetSingleChar hnd prompt = do
   hPutStr stdout prompt
   hFlush stdout
   Ex.bracket (hGetBuffering hnd) (hSetBuffering hnd) $ \_ -> do
      hSetBuffering hnd NoBuffering
      c <- hGetChar hnd
      hPutStrLn stdout ""
      return (Just c)

basicGetInput :: Handle -> String -> IO (Maybe String)
basicGetInput hnd prompt = do
   hPutStr stdout prompt
   hFlush stdout
   x <- hGetLine hnd
   return (Just x)

basicOutput :: BackendOutput -> IO ()
basicOutput (RegularOutput out) = hPutStr stdout out
basicOutput (InfoOutput out)    = hPutStr stdout out
basicOutput (ErrorOutput out)   = hPutStr stderr out
