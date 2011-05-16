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

-- using app-funcs - neater and more limited than monads, more func/decl not imperative like do-notation
-- only need full power of monads when assigning a variable
import Control.Applicative
import Text.Parsec
import Utilities

type Parser = Parsec String ()

-- | parser top level
ionTop :: Parser ()
ionTop = string "Hello\n" *> eof

-- | parses the filename and returns the result if sucessful
-- | maybe move into main
ionParse :: FilePath -> String -> MExcept ()
ionParse fileName fileData =
    -- do  parseRes <- parseFromFile odeMain fileName
    case parseRes of
        Left err    -> Left ("parse error at " ++ show err)
        Right res   -> Right res
  where
    parseRes = parse ionTop fileName fileData
