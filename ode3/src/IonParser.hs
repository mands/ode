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
import Text.Parsec hiding (many)
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language( javaStyle )
import Text.Parsec.Perm

import Utilities
import qualified IonAST as I

-- hijack the javaStyle default definition, gives us a bunch of ready-made parsers
ionLangDef = javaStyle
    {
        -- add more later
        P.reservedNames = ["channel", "density", "equilibrium_potential", "subunits", "open_states",
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
float      = P.float lexer
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

-- thankfully, we don't need any expressions in this language

-- | a parameterised single attribute parser for a given attriibute identifier
-- TODO - fix the comma separated list of attribute, commaSep?
-- attrib :: String -> Parser String
attrib res p = reserved res *> colon *> p <* skipMany comma

-- only unidirectional reactions for now
ionReaction :: Parser I.StateReaction
ionReaction = braces (I.StateReaction <$> identifier <*> (reservedOp "->" *> identifier) <*>
            (comma *> attrib "rate" float))

-- | Flexible perm parser for channel attributes - only prob is name, could place into the parser state
ionChannelBody :: Parser I.IonChannel
ionChannelBody =
    permute (t  <$$> (attrib "density" float)
                <||> (attrib "equilibrium_potential" float)
                <||> (attrib "subunits" integer)
                <||> (attrib "open_states" (braces (commaSep1 identifier)))
                <||> (attrib "states" (braces (commaSep1 ionReaction)))
            )
  where
    t d e s os ss = I.IonChannel "" d e s os ss

ionChannelDef :: Parser I.IonChannel
ionChannelDef = updateName <$> (reserved "channel" *> identifier) <*> braces ionChannelBody
  where
    -- need to update the model with the name, maybe easier to use state or
    updateName n model = model { I.name = n}

-- | parser top level
ionTop :: Parser [I.IonChannel]
ionTop = many ionChannelDef <* eof

-- | parses the string and returns the result if sucessful
-- | maybe move into main
-- | TODO - switch to bytestring
ionParse :: FilePath -> String -> MExcept [I.IonChannel]
ionParse fileName fileData =
    -- do  parseRes <- parseFromFile odeMain fileName
    case parseRes of
        Left err    -> Left ("parse error at " ++ show err)
        Right res   -> Right res
  where
    parseRes = parse ionTop fileName fileData
