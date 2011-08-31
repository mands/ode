-----------------------------------------------------------------------------
--
-- Module      :  Ode.Parser
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |Parser for the Ode3 language, used to describe stochastic-hybrid systesm comprising of
-- chemical kinetic reactions and ODEs only (for now)
-- TO ADD
-- * module system - DONE!
-- * SDEs?
-- * units?
-- * types
-- * first-class/nested component definitions?
-- * sub-modules?
-- * many more...
--
-----------------------------------------------------------------------------

module Ode.Parser (
    odeParse,
) where

import Control.Applicative
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language( javaStyle )
import Text.Parsec.Perm

import Utils.Utils
import qualified Ode.AST as O

-- |hijack the javaStyle default definition, gives us a bunch of ready-made parsers/behaviours
coreLangDef = javaStyle
    {
        -- add more later
        T.reservedNames =   ["module", "component", "return",
                            "val", "init",
                            "ode", "delta",
                            "rre", "reaction", "rate",
                            "default",
                            "true", "false"],
        -- unary ops and relational ops?
        -- do formatting operators count? e.g. :, {, }, ,, ..,  etc.
        -- NO - they are symbols to aid parsiing and have no meaning in the language itself...
        T.reservedOpNames = ["=",
                            "*", "/", "%", "+", "-",
                            "<", "<=", ">", ">=", "==", "!=",
                            "&&", "||", "!", "and", "or", "not"
                            ],
        T.caseSensitive = True
    }

lexer :: T.TokenParser ()
lexer  = T.makeTokenParser coreLangDef

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

-- |parses a upper case identifier
upperIdentifier :: Parser String
upperIdentifier = (:) <$> upper <*> many alphaNum <?> "module identifier"

-- |lexeme parser for module identifier
modIdentifier :: Parser String
modIdentifier = lexeme upperIdentifier

-- |parses a module element reference, e.g. A.x
modElemIdentifier :: Parser O.ModLocalId
modElemIdentifier  = lexeme (O.ModId <$> upperIdentifier <*> (char '.' *> identifier))

-- |parse either a local or module id e.g. A.x or x
modLocalIdentifier :: Parser O.ModLocalId
modLocalIdentifier =    try modElemIdentifier
                        <|> O.LocalId <$> identifier <?> "local or module identifier"

-- |comma sepated parameter list of any parser, e.g. (a,b,c)
paramList = parens . commaSep

-- |a parameterised single attribute parser for a given attriibute identifier
-- TODO - fix the comma separated list of attribute, commaSep?
-- attrib :: String -> Parser String
attrib res p = reserved res *> colon *> p <* optional comma

-- |a basic numeric expression, using parsec expression builder
compExpr  :: Parser O.Expr
compExpr  =  buildExpressionParser exprOpTable compTerm <?> "expression"

-- |Expression operator precedence table
-- TODO - add parens and commas to the expressions?
-- exprOpTable :: OperatorTable String () Identity O.Expr
exprOpTable =
    [
    [prefix "-" O.Neg, prefix "!" O.Not, prefix "not" O.Not]
    ,[binary "*" O.Mul AssocLeft, binary "/" O.Div AssocLeft, binary "%" O.Mod AssocLeft]
    ,[binary "+" O.Add AssocLeft, binary "-" O.Sub AssocLeft]
    ,[binary "<" O.LT AssocLeft, binary "<=" O.LE AssocLeft, binary ">" O.GT AssocLeft, binary ">=" O.GE AssocLeft]
    ,[binary "==" O.EQ AssocLeft, binary "!=" O.NEQ AssocLeft]
    ,[binary "&&" O.And AssocLeft, binary "and" O.And AssocLeft]
    ,[binary "||" O.Or AssocLeft, binary "or" O.Or AssocLeft]
    ]
  where
    binary name binop assoc = Infix (reservedOp name *> pure (\a b -> O.BinExpr binop a b) <?> "binary operator") assoc
    prefix name unop         = Prefix (reservedOp name *> pure (\a -> O.UnExpr unop a) <?> "unary operator")

-- |parser for a numerical sequence, e.g. [a, b .. c]
-- where a is the start, b is the next element, and c is the stop
numSeqTerm :: Parser O.Expr
numSeqTerm = createSeq <$> number <*> (comma *> number) <*> (symbol ".." *> number) <?> "numerical sequence"
  where
    createSeq a b c = O.NumSeq a b c -- (b-a) c

--boolTerm :: Parser Bool
--boolTerm =  try <$> reserved "true" *> True
--            <|> reserved "false"  *> False
--            <?> "boolean"

piecewiseTerm :: Parser O.Expr
piecewiseTerm = O.Piecewise <$> (endBy1 ((,) <$> compExpr <*> (colon *> compExpr)) comma)
                            <*> (reserved "default" *> colon *> compExpr)

-- |parse a term - the value on either side of an operator
-- should ODEs be here - as terms or statements?
compTerm :: Parser O.Expr
compTerm =  parens compExpr
            <|> O.Number <$> number
--            <|> O.Boolean <$> boolTerm
            <|> try (brackets numSeqTerm)
            <|> try (braces piecewiseTerm)
            <|> try (O.Call <$> modLocalIdentifier <*> paramList compExpr)
            <|> O.ValueRef <$> modLocalIdentifier
            <?> "valid term"

{-
TODO - File GHC/Parsec bug

odeDef :: (Double -> O.Expr -> O.CompStmt) -> Parser O.CompStmt
odeDef n = permute (n
            <$$> (attrib "init" number)
            <||> (attrib "delta" compExpr)
            )
            -- <|> ((O.OdeDef <$> (reserved "ode" *> identifier <* reservedOp "=" )) >>= (\n -> braces (odeDef n)))
-}

-- |parse an ode attribute definition
odeDef = permute (O.OdeDef ""
            <$$> (attrib "init" number)
            <||> (attrib "delta" compExpr)
            ) <?> "ode definition"

-- |parse a rre attribute definition
rreDef = permute (O.RreDef ""
            <$$> (attrib "reaction" ((,) <$> identifier <*> (reservedOp "->" *> identifier)))
            <||> (attrib "rate" compExpr)
            ) <?> "rre definition"

-- |parse a value definition
-- e.g., val x = expr
valueDef :: Parser O.ValueDef
valueDef = O.ValueDef   <$> (reserved "val" *> commaSep1 identifier) <*> (reservedOp "=" *> compExpr)

-- |parser for the statements allowed within a component body
compStmt :: Parser O.CompStmt
compStmt =  --O.CompCallDef <$> commaSep1 identifier <*> (reservedOp "=" *> identifier) <*> paramList compExpr
            O.CompValue <$> valueDef
            <|> O.InitValueDef <$> (reserved "init" *> commaSep1 identifier) <*> (reservedOp "=" *> compExpr)
            <|> updateOde <$> (reserved "ode" *> identifier) <*> (reservedOp "=" *> braces odeDef)
            <|> updateRre <$> (reserved "rre" *> identifier) <*> (reservedOp "=" *> braces rreDef)
            <?> "valid component statement"
  where
    updateOde n ode = ode {O.odeName = n}
    updateRre n rre = rre {O.rreName = n}

-- |parser for the component body, a list of statements and return expressions
compBody :: Parser ([O.CompStmt], [O.Expr])
compBody = (,)  <$> many compStmt --compStmt `endBy` lexeme newline --
                <*> (reserved "return" *> paramList compExpr)

-- |parser for defining a component, where either a defintion or module parameter component may follow
compDef :: Parser O.Component
compDef = do
    cName <- reserved "component" *> identifier
    compParse cName
  where
    compParse cName =
        O.ComponentRef <$> pure cName <*> (reservedOp "=" *> modElemIdentifier)
        <|> (uncurry <$> (O.Component <$> pure cName <*> paramList identifier) <*> braces compBody)
        <?> "component definition"

-- |parse the body of a module
moduleBody :: Parser O.ModuleElem
moduleBody =    O.ModuleElemComponent <$> compDef
                <|> O.ModuleElemValue <$> valueDef
                <?> "component or value defintion"

-- |parse a chain/tree of module applications
moduleAppParams :: Parser O.ModuleAppParams
moduleAppParams = O.ModuleAppParams <$> modIdentifier <*> optionMaybe (paramList moduleAppParams)

-- |parse a module, either an entire definition/abstraction or an application
moduleDef :: Parser O.Module
moduleDef = do
    mName <- reserved "module" *> modIdentifier
    modParse mName
  where
    modParse mName =
        O.ModuleApp <$> pure mName <*> (reservedOp "=" *> moduleAppParams)
        <|> O.ModuleAbs <$> pure mName <*> optionMaybe (paramList modIdentifier) <*> braces (many1 moduleBody)
        <?> "module definition"

-- |parse the open directive
moduleOpen :: Parser O.FileOpen
moduleOpen = reserved "open" *> stringLiteral

-- |top level parser for a file
odeTop :: Parser O.Model
odeTop = O.Model <$> (whiteSpace *> many moduleOpen) <*> many1 moduleDef <* eof

-- |parses the string and returns the result if sucessful
-- maybe move into main
-- TODO - switch to bytestring?
odeParse :: FilePath -> String -> MExcept O.Model
odeParse fileName fileData =
    -- do  parseRes <- parseFromFile odeMain fileName
    case parseRes of
        Left err    -> Left ("parse error at " ++ show err)
        Right res   -> Right res
  where
    parseRes = parse odeTop fileName fileData
