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
module Parser.Common
where

import Control.Applicative
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
-- import Text.Parsec.String
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified AST.Module as MA
import AST.Common

-- import {-# SOURCE #-} Lang.Common.Ops (opReservedNames) -- special import to break import cycle

type Parser = Parsec String ()

-- Default Parser style ------------------------------------------------------------------------------------------------
commonLangDef = emptyDef
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
    , T.reservedNames = [
                        -- module lang
                          "module", "import", "as", "let"
                        -- main ode lang
                        , "component", "where", "return"
                        , "val", "init"
                        , "ode", "delta", "deltaVal"
                        , "sde", "drift", "diffusion", "weiner"
                        , "ssa", "reaction", "rate"
                        , "piecewise", "default"
                        , "True", "False", "time", "None"
                        -- units lang
                        , "quantity", "dim", "unit", "SI", "alias"
                        , "conversion", "factor", "convert"
                        -- type lang
                        , "type", "wrap", "unwrap"
                        -- ion...implemented externally
                        ] ++ opReservedNames
    -- unary ops and relational ops?
    -- do formatting operators count? e.g. :, {, }, ,, ..,  etc.
    -- NO - they are symbols to aid parsiing and have no meaning in the language itself...
    , T.reservedOpNames =   [ "=", "=>", "()", "_"
                            , "*", "/", "%", "+", "-"
                            , "<", "<=", ">", ">=", "==", "!="
                            , "&&", "||", "!", "and", "or", "not"
                            , "::", "^", "#"
                            ]
    -- need case-sens for type and unit declarations
    , T.caseSensitive = True
    }

opReservedNames = [
                -- math ops
                "sin", "cos", "tan", "sincos"
                , "asin", "acos", "atan", "atan2"
                , "exp", "exp2", "exp10", "pow10"
                , "log", "log2", "log10", "logb"
                , "pow", "sqrt", "cbrt"
                , "hypot", "expm1", "log1p"
                , "sinh", "cosh", "tanh", "asinh", "acosh", "atanh"
                , "erf", "erfc", "lgamma", "gamma", "tgamma"
                -- other ops
                , "upow", "uroot", "abs", "floor", "ceil", "round"  -- "rand"
                ]

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser commonLangDef
-- lexer  = T.makeTokenParser emptyDef

-- Shared Lexical Combinators ------------------------------------------------------------------------------------------

-- For efficiency, we will bind all the used lexical parsers at toplevel.
whiteSpace  = T.whiteSpace lexer
lexeme      = T.lexeme lexer
symbol      = T.symbol lexer
stringLiteral = T.stringLiteral lexer
natural     = T.natural lexer
integer     = T.integer lexer
float       = T.float lexer
natFloat    = T.naturalOrFloat lexer
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
angles      = T.angles lexer
dot         = T.dot lexer

-- | parses a upper case identifier
upperIdentifier :: Parser String
upperIdentifier = lexeme ((:) <$> upper <*> many alphaNum <?> "capitalised identifier")

-- | parses a case-insensitive, alpha-only identifier
alphaIdentifier :: Parser String
alphaIdentifier = lexeme $ many1 letter

-- | comma sepated parameter list of any parser, e.g. (a,b,c)
paramList = parens . commaSep1


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

--
---- | Code to properly handle +/- signed numbers, both interger and floating
--data Sign = Positive | Negative
--
--applySign :: Num a => Sign -> a -> a
--applySign Positive = id
--applySign Negative = negate
--
--sign :: Parser Sign
--sign =  (char '-' >> return Negative)
--        <|> (char '+' >> return Positive)
--        <|> return Positive
--
--number' :: Parser Double
--number' =  do
--    s   <- sign
--    num <- natFloat
--    return $ case num of
--        Left  x -> fromInteger $ applySign s x
--        Right x -> applySign s x


-- Shared Module Parsers -----------------------------------------------------------------------------------------------
-- | lexeme parser for module identifier, return a list of module URI elements
modIdentifier :: Parser ModURIElems
modIdentifier = upperIdentifier `sepBy` (char '.')

singModId :: Parser ModName
singModId = ModName <$> upperIdentifier

-- | commands to import modules into the system, either globally or within module
importCmd :: Parser ModImport
importCmd = try importAll
            <|> importSing
  where
    -- need a monad, not applicative, to modify the state
    importAll = do
        modRoot <- mkModRoot <$> (reserved "import" *> modPathImportAll)
        -- addImport modRoot
        return $ ModImport modRoot Nothing

    modPathImportAll :: Parser ModURIElems
    modPathImportAll = upperIdentifier `sepEndBy` (char '.') <* (char '*')

    importSing = do
        modURI <- (reserved "import" *> modPathImport)
        mAlias <- optionMaybe (reserved "as" *> singModId)
        let modRoot = mkModRoot $ List.init modURI
        -- addImport modRoot
        return $ ModImport modRoot (Just [(ModName $ List.last modURI, mAlias)])

    -- | lexeme parser for a module string in dot notation
    modPathImport :: Parser ModURIElems
    modPathImport = upperIdentifier `sepBy1` (char '.')

    -- add modURI to set
    -- addImport modURI = modifyState (\s -> s { stImports = Set.insert modURI (stImports s) } )
