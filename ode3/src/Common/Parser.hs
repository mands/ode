-----------------------------------------------------------------------------
--
-- Module      :  Common.Parser
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Common parsing logic and contructs to all parsers
--
-----------------------------------------------------------------------------

-- export everything
module Common.Parser
where

import Control.Applicative
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Language( javaStyle )
import qualified Text.Parsec.Token as T
import Text.Parsec.String

-- Default Parser style
-- | hijack the javaStyle default definition, gives us a bunch of ready-made parsers/behaviours
commonLangDef = javaStyle
    {
        -- add more later
        T.reservedNames =   [
                            -- module lang
                            "module", "import", "as",
                            -- main ode lang
                            "component", "where",
                            "val", "init", "sval",
                            "ode", "delta",
                            "rre", "reaction", "rate",
                            "default",
                            "True", "False", "time"
                            -- ion...implemented externally
                            ],

        -- unary ops and relational ops?
        -- do formatting operators count? e.g. :, {, }, ,, ..,  etc.
        -- NO - they are symbols to aid parsiing and have no meaning in the language itself...
        T.reservedOpNames = ["=", "=>", "()", "_",
                            "*", "/", "%", "+", "-",
                            "<", "<=", ">", ">=", "==", "!=",
                            "&&", "||", "!", "and", "or", "not"
                            ],
        T.caseSensitive = True
    }

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser commonLangDef

-- For efficiency, we will bind all the used lexical parsers at toplevel.
whiteSpace  = T.whiteSpace lexer
lexeme      = T.lexeme lexer
symbol      = T.symbol lexer
stringLiteral = T.stringLiteral lexer
natural     = T.natural lexer
integer     = T.integer lexer
float       = T.float lexer
parens      = T.parens lexer
semi        = T.semi lexer
colon       = T.colon lexer
comma       = T.comma lexer
identifier  = T.identifier lexer
reserved    = T.reserved lexer
reservedOp  = T.reservedOp lexer
commaSep    = T.commaSep lexer
commaSep1   = T.commaSep1 lexer
braces      = T.braces lexer
brackets    = T.brackets lexer
dot         = T.dot lexer

-- |number parser, parses most formats
number :: Parser Double
number =    try float
            <|> fromIntegral <$> integer
            <?> "number"

-- | boolean parser, parses a case-sensitive, boolean literal
boolean :: Parser Bool
boolean =  reserved "True" *> pure True
            <|> reserved "False"  *> pure False
            <?> "boolean"

-- | time term, is a special identifier
time :: Parser ()
time = reserved "time" *> pure ()

-- | unit term, is a special identifier
unit :: Parser ()
unit = reservedOp "()" *> pure ()

-- | lexeme parser for module identifier
modIdentifier :: Parser String
modIdentifier = lexeme upperIdentifier

-- | lexeme parser for a module string in dot notation
modPathIdentifier :: Parser [String]
modPathIdentifier = lexeme $ upperIdentifier `sepBy1` (char '.')

-- | parses a upper case identifier
upperIdentifier :: Parser String
upperIdentifier = (:) <$> upper <*> many alphaNum <?> "module identifier"

-- | comma sepated parameter list of any parser, e.g. (a,b,c)
paramList = parens . commaSep1

-- | tuple, requires at least two values, comma separated
tuple :: Parser a -> Parser [a]
tuple p = parens $ (:) <$> (p <* comma) <*> commaSep1 p

-- | used to parse a single element by itself, a, or contained eithin a comma-sep list, (a,...)
singOrList :: Parser a -> Parser [a]
singOrList p = try ((\p -> p:[]) <$> (p))
                <|> paramList p
                <?> "single element or list"

-- |a parameterised single attribute parser for a given attribute identifier
-- TODO - fix the comma separated list of attribute, commaSep?
-- attrib :: String -> Parser String
attrib res p = reserved res *> colon *> p <* optional comma

