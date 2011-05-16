-----------------------------------------------------------------------------
--
-- Module      :  IonParser
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Ion Channel front-end langauge definition and parser
--
-----------------------------------------------------------------------------

module IonParser (
ionParse,
) where

-- parsec compat
import Text.ParserCombinators.Parsec

import Utilities

-- | parser top level
ionMain :: Parser ()
ionMain =
    do  string "Hello\n"
        eof
        return ()

-- | parses the filename and returns the result if sucessful
-- | maybe move into main
ionParse :: FilePath -> String -> MExcept ()
ionParse fileName fileData =
    -- do  parseRes <- parseFromFile odeMain fileName
    case parseRes of
        Left err    -> Left ("parse error at " ++ show err)
        Right res   -> Right res
  where
    parseRes = parse ionMain fileName fileData
