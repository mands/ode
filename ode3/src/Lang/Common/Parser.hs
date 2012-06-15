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
module Lang.Common.Parser
where

import Control.Applicative
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Language( javaStyle )
import qualified Text.Parsec.Token as T
-- import Text.Parsec.String
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Lang.Module.AST as MA
import Lang.Common.AST

type Parser = Parsec String ()

-- Default Parser style ------------------------------------------------------------------------------------------------

-- | hijack the javaStyle default definition, gives us a bunch of ready-made parsers/behaviours
commonLangDef = javaStyle
    {
        -- add more later
        T.reservedNames =   [
                            -- module lang
                            "module", "import", "as", "let",
                            -- main ode lang
                            "component", "where",
                            "val", "init", "sval",
                            "ode", "delta",
                            "rre", "reaction", "rate",
                            "default",
                            "True", "False", "time",
                            -- units lang
                            "quantity", "dim", "unit", "SI", "alias"
                            -- ion...implemented externally
                            ],

        -- unary ops and relational ops?
        -- do formatting operators count? e.g. :, {, }, ,, ..,  etc.
        -- NO - they are symbols to aid parsiing and have no meaning in the language itself...
        T.reservedOpNames = ["=", "=>", "()", "_",
                            "*", "/", "%", "+", "-",
                            "<", "<=", ">", ">=", "==", "!=",
                            "&&", "||", "!", "and", "or", "not",
                            "::"
                            ],
        T.caseSensitive = True
    }

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser commonLangDef

-- Shared Lexical Combinators ------------------------------------------------------------------------------------------

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

-- | parses a upper case identifier
upperIdentifier :: Parser String
upperIdentifier = (:) <$> upper <*> many alphaNum <?> "capitalised identifier"

-- | comma sepated parameter list of any parser, e.g. (a,b,c)
paramList = parens . commaSep1


-- Shared Module Parsers -----------------------------------------------------------------------------------------------

-- | lexeme parser for module identifier, return a list of module URI elements
modIdentifier :: Parser ModURIElems
modIdentifier = lexeme $ upperIdentifier `sepBy` (char '.')

singModId :: Parser ModName
singModId = ModName <$> lexeme upperIdentifier

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
    modPathImportAll = lexeme $ upperIdentifier `sepEndBy` (char '.') <* (char '*')

    importSing = do
        modURI <- (reserved "import" *> modPathImport)
        mAlias <- optionMaybe (reserved "as" *> singModId)
        let modRoot = mkModRoot $ List.init modURI
        -- addImport modRoot
        return $ ModImport modRoot (Just [(ModName $ List.last modURI, mAlias)])

    -- | lexeme parser for a module string in dot notation
    modPathImport :: Parser ModURIElems
    modPathImport = lexeme $ upperIdentifier `sepBy1` (char '.')

    -- add modURI to set
    -- addImport modURI = modifyState (\s -> s { stImports = Set.insert modURI (stImports s) } )

