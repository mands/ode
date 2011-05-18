-----------------------------------------------------------------------------
--
-- Module      :  Core.Parser
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Parser for the Core Ode3 language, used to describe stochastic-hybrid systesm comprising of
-- | chemical kinetic reactions and ODEs only (for now)
-- | TO ADD
-- | * module system
-- | * SDEs?
-- | * units?
-- | * types
-- | * many more...
--
-----------------------------------------------------------------------------

module Core.Parser (
    coreParse,
) where

import Control.Applicative
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language( javaStyle )
import Text.Parsec.Perm

import Utilities
import qualified Core.AST as C

-- hijack the javaStyle default definition, gives us a bunch of ready-made parsers
coreLangDef = javaStyle
    {
        -- add more later
        P.reservedNames = ["component", "ode", "init", "delta", "return"],
        -- unary ops and relational ops?
        -- do formatting operators count? e.g. :, {, }, ,, etc.
        P.reservedOpNames = ["*","/","+","-", "="]
    }

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser coreLangDef

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

-- comma sepated parameter list of any parser, e.g. (a,b,c)
paramList = parens . commaSep

-- expressions - use parsec experssion builder
-- | a basic numeric expression
compExpr  :: Parser C.Expr
compExpr  =  buildExpressionParser exprOpTable compTerm <?> "expression"

--exprOpTable :: OperatorTable String () Identity C.Expr
exprOpTable =
    [[binary "*" C.Mul AssocLeft, binary "/" C.Div AssocLeft]
    ,[binary "+" C.Add AssocLeft, binary "-" C.Sub AssocLeft]
    ]
  where
    binary name binop assoc = Infix (reservedOp name *> pure (\a b -> C.BinExpr a binop b) <?> "operator") assoc
    prefix name fun         = Prefix (reservedOp name *> fun)

-- should ODEs be here - as terms or statements?
-- | term - the value on either side of an operator
compTerm :: Parser C.Expr
compTerm =  parens compExpr
            <|> C.Number <$> float
            <|> try (C.FuncCall <$> identifier <*> paramList compExpr)
            <|> C.ValueRef <$> identifier
            <?> "term"

-- | general statements allowed within a component body
compStmt :: Parser C.CompStmt
compStmt =  C.CompCallDef <$> paramList identifier <*> (reservedOp "=" *> identifier) <*> paramList compExpr
            <|> C.ValueDef <$> identifier <*> (reservedOp "=" *> compExpr)
            <?> "statement"

-- body is a list of statements and return expressions
compBody :: Parser ([C.CompStmt], [C.Expr])
compBody = (,)  <$> compStmt `endBy` semi
                <*> (reserved "return" *> paramList compExpr)

compDef :: Parser C.Component
compDef = uncurry   <$> (C.Component <$> (reserved "component" *> identifier) <*> paramList identifier)
                    <*> braces compBody

coreTop :: Parser [C.Component]
coreTop = whiteSpace *> many1 compDef <* eof

-- | parses the string and returns the result if sucessful
-- | maybe move into main
-- | TODO - switch to bytestring
coreParse :: FilePath -> String -> MExcept [C.Component]
coreParse fileName fileData =
    -- do  parseRes <- parseFromFile odeMain fileName
    case parseRes of
        Left err    -> Left ("parse error at " ++ show err)
        Right res   -> Right res
  where
    parseRes = parse coreTop fileName fileData
