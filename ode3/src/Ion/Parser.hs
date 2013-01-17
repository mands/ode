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
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Text.Parsec.Perm

import Utils.Utils
import qualified Ion.AST as I
import AST.CoreFlat(SimType(..))

-- type Parser = Parsec String ()

-- |Ion Channel parser style
ionLangDef = emptyDef
    { T.commentStart    = "/*"
    , T.commentEnd      = "*/"
    , T.commentLine     = "//"

    -- reuse the initial identifier parsers
    -- , T.identStart     = letter <|> char '_'
    -- , T.identLetter    = alphaNum <|> oneOf "_'"

    -- no user-defined operators
    , T.opStart = oneOf ""
    , T.opLetter = oneOf ""

    -- add more later
    , T.reservedNames = [ "channel", "density", "equilibrium_potential"
                        , "subunits", "initial_state", "open_states", "additional_inputs"
                        , "transitions", "transition", "f_rate", "r_rate"
                        , "ode", "sde", "rre"
                        ]
    -- unary ops and relational ops?
    -- do formatting operators count? e.g. :, {, }, ,, etc.
    , T.reservedOpNames = ["<->"] --, "<-", "<->", ":", "{", "}", ","]

    -- need case-sens for type and unit declarations
    , T.caseSensitive = True
    }

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser ionLangDef

-- thankfully, we don't need any expressions in this language

-- For efficiency, we will bind all the used lexical parsers at toplevel.
whiteSpace  = T.whiteSpace lexer
natural     = T.natural lexer
integer     = T.integer lexer
float       = T.float lexer
parens      = T.parens lexer
colon       = T.colon lexer
comma       = T.comma lexer
identifier  = T.identifier lexer
reserved    = T.reserved lexer
reservedOp  = T.reservedOp lexer
braces      = T.braces lexer


-- | boolean parser, parses a case-sensitive, boolean literal
boolean :: Parser Bool
boolean =  reserved "True" *> pure True
            <|> reserved "False"  *> pure False
            <?> "boolean"

-- | number parser, parses most formats
number :: Parser Double
number =    try float
            <|> fromIntegral <$> integer
            <?> "number"


-- | parse a macro representing an syntantically valid Ode numeric expression (such that we can embed in Ode output later)
-- no newlines allowed
exprMacro :: Parser String
exprMacro = many (alphaNum <|> char ' ' <|> oneOf odeOps) -- Till anyChar (try (comma <|> string "}"))
  where
    odeOps = "().!*/+<>-=&|" -- brackets, fp dot, ode math ops

-- |a more flexible list separater, allows optional end comma as needed for permutation lists
listSep p = sepEndBy1 p comma

-- | Wrapper around our default attribute notation
attribDef p = braces (permute p)

-- |a parameterised single attribute parser for a given attriibute identifier
-- TODO - fix the comma separated list of attribute, commaSep?
-- attrib :: String -> Parser String
attrib res p = reserved res *> colon *> p <* optional comma


-- |parser for a single, unidirectional reaction, e.g. A->B
ionTransition :: Parser I.Transition
ionTransition = mkTransition <$> attribDef transitionAttribs
  where
    mkTransition ((a, b), fRate, rRate) = I.Transition a b fRate rRate
    transitionAttribs = (,,)    <$$> attrib "transition" ((,) <$> identifier <*> (reservedOp "<->" *> identifier))
                                <||> attrib "f_rate" (I.ExprMacro <$> exprMacro)
                                <||> attrib "r_rate" (I.ExprMacro <$> exprMacro)


-- |parser for a channel defintion
-- records the name first and uses record update syntax to update the ionChannelBody parser
-- NOTE - we can't pass along a curried-constructor to the attrib parser, causes GHC int. error as in Ode Parser
ionChannelDef :: Parser I.IonChannel
ionChannelDef = do
    iName <- reserved "channel" *> identifier
    ionChannel <- attribDef ionChannelBody
    return $ (ionChannel { I.name = iName, I.inputs = ("V" : I.inputs ionChannel) })
  where
    -- |flexible permutation parser for channel attributes
    -- only prob is recording the name, could place into the parser state
    ionChannelBody = I.mkIonChannel     <$?> (SimODE, attrib "sim_type" ionSimType)
                                        <||> (attrib "density" number)
                                        <||> (attrib "equilibrium_potential" number)
                                        <||> (attrib "channel_conductance" number)
                                        <|?> (1, attrib "subunits" natural) -- NYI
                                        <|?> ([], attrib "additional_inputs" (braces (listSep identifier)))
                                        <||> (attrib "initial_state" identifier)
                                        <||> (attrib "open_states" (braces (listSep identifier)))
                                        <||> (attrib "transitions" (braces (listSep ionTransition)))

    ionSimType :: Parser SimType
    ionSimType =    reserved "ode" *> pure SimODE
                    <|> reserved "sde" *> pure SimSDE
                    <|> reserved "rre" *> pure SimRRE
                    <?> "simulation type"

-- |parser top level
ionTop :: Parser [I.IonChannel]
ionTop = whiteSpace *> many1 ionChannelDef <* eof

-- | parses the string and returns the result if sucessful
-- maybe move into main
-- TODO - switch to bytestring/Data.Text
ionParse :: FilePath -> String -> MExcept [I.IonChannel]
ionParse fileName fileData =
    -- do  parseRes <- parseFromFile odeMain fileName
    case parseRes of
        Left err    -> Left ("parse error at " ++ show err)
        Right res   -> Right res
  where
    parseRes = parse ionTop fileName fileData
