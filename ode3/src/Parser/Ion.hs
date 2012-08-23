-----------------------------------------------------------------------------
--
-- Module      :  IonParser
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Ion Channel front-end langauge definition and parser
-- using app-funcs - neater and more limited than monads, more func/decl not imperative like do-notation
-- only need full power of monads when assigning a variable
--
-----------------------------------------------------------------------------

module Ion.Parser (
    ionParse,
) where

import Control.Applicative
import Text.Parsec hiding (many, optional)
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language( javaStyle )
import Text.Parsec.Perm

import Utils.Utils
import qualified Ion.AST as I

-- |hijack the javaStyle default definition, gives us a bunch of ready-made parsers
ionLangDef = javaStyle
    {
        -- add more later
        P.reservedNames = ["channel", "density", "equilibrium_potential", "subunits", "open_states",
            "states", "rate", "forward_rate", "reverse_rate"],
        -- unary ops and relational ops?
        -- do formatting operators count? e.g. :, {, }, ,, etc.
        P.reservedOpNames = ["->"] --, "<-", "<->", ":", "{", "}", ","]
    }

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser ionLangDef

-- thankfully, we don't need any expressions in this language

-- For efficiency, we will bind all the used lexical parsers at toplevel.
whiteSpace  = P.whiteSpace lexer
natural     = P.natural lexer
integer     = P.integer lexer
float      = P.float lexer
parens      = P.parens lexer
colon       = P.colon lexer
comma       = P.comma lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
braces      = P.braces lexer

-- |a more flexible list separater, allows optional end comma as needed for permutation lists
listSep p = sepEndBy1 p comma

-- |a parameterised single attribute parser for a given attriibute identifier
-- TODO - fix the comma separated list of attribute, commaSep?
-- attrib :: String -> Parser String
attrib res p = reserved res *> colon *> p <* optional comma

-- |parser for a single, unidirectional reaction, e.g. A->B
ionReaction :: Parser I.StateReaction
ionReaction = braces (I.StateReaction <$> identifier <*> (reservedOp "->" *> identifier) <*>
            (comma *> attrib "rate" float))

-- |flexible permutation parser for channel attributes
-- only prob is recording the name, could place into the parser state
ionChannelBody :: Parser I.IonChannel
ionChannelBody = permute (I.IonChannel ""
                            <$$> (attrib "density" float)
                            <||> (attrib "equilibrium_potential" float)
                            <||> (attrib "subunits" integer)
                            <||> (attrib "open_states" (braces (listSep identifier)))
                            <||> (attrib "states" (braces (listSep ionReaction)))
                            )

-- |parser for a channel defintion
-- records the name first and uses record update syntax to update the ionChannelBody parser
ionChannelDef :: Parser I.IonChannel
ionChannelDef = updateName <$> (reserved "channel" *> identifier) <*> braces ionChannelBody
  where
    -- need to update the model with the name, maybe easier to use state or bind name outside the channel
    updateName n model = model { I.name = n }

-- |parser top level
ionTop :: Parser [I.IonChannel]
ionTop = whiteSpace *> many1 ionChannelDef <* eof

-- | parses the string and returns the result if sucessful
-- maybe move into main
-- TODO - switch to bytestring
ionParse :: FilePath -> String -> MExcept [I.IonChannel]
ionParse fileName fileData =
    -- do  parseRes <- parseFromFile odeMain fileName
    case parseRes of
        Left err    -> Left ("parse error at " ++ show err)
        Right res   -> Right res
  where
    parseRes = parse ionTop fileName fileData
