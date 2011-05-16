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
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language( javaStyle )

import Utilities

-- hijack the javaStyle default definition, gives us a bunch of ready-made parsers
ionLangDef = javaStyle
    {
        -- add more later
        P.reservedNames = ["channel", "density", "equibilrium_potential", "subunits", "open_states",
            "states", "rate", "forward_rate", "reverse_rate"],
        -- unary ops and relational ops?
        -- do formatting operators count? e.g. :, {, }, ,, etc.
        P.reservedOpNames = ["->", "<-", "<->", ":", "{", "}", ","]
    }

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser ionLangDef

-- For efficiency, we will bind all the used lexical parsers at toplevel.
whiteSpace  = P.whiteSpace lexer
lexeme      = P.lexeme lexer
symbol      = P.symbol lexer
natural     = P.natural lexer
integer     = P.integer lexer
float'      = P.float lexer
parens      = P.parens lexer
semi        = P.semi lexer
colon       = P.colon lexer
comma       = P.comma lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
commaSep    = P.commaSep lexer
commaSep1   = P.commaSep1 lexer
braces      = P.braces lexer

-- we don't need any expressions in this language

-- test ap parser, bit convoluted but works!
ionChannelParam :: String -> Parser (String, Integer)
ionChannelParam param = (,) (param) <$> (reserved param *> colon *> integer)

-- | parser top level
ionTop :: Parser ()
ionTop = string "Hello" *> whiteSpace *> eof

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
