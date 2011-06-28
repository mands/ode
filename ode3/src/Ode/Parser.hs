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
-- | * module system - DONE!
-- | * SDEs?
-- | * units?
-- | * types
-- | * first-class/nested component definitions?
-- | * sub-modules?
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
import qualified Text.Parsec.Token as T
import Text.Parsec.Language( javaStyle )
import Text.Parsec.Perm

import Utilities
import qualified Core.AST as C

-- |hijack the javaStyle default definition, gives us a bunch of ready-made parsers/behaviours
coreLangDef = javaStyle
    {
        -- add more later
        T.reservedNames =   ["module", "component", "return",
                            "val", "init",
                            "ode", "delta",
                            "rre", "reaction", "rate",
                            "default"],
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
modElemIdentifier :: Parser C.ModLocalId
modElemIdentifier  = lexeme (C.ModId <$> upperIdentifier <*> (char '.' *> identifier))

-- |parse either a local or module id e.g. A.x or x
modLocalIdentifier :: Parser C.ModLocalId
modLocalIdentifier =    try modElemIdentifier
                        <|> C.LocalId <$> identifier <?> "local or module identifier"

-- |comma sepated parameter list of any parser, e.g. (a,b,c)
paramList = parens . commaSep

-- |a parameterised single attribute parser for a given attriibute identifier
-- TODO - fix the comma separated list of attribute, commaSep?
-- attrib :: String -> Parser String
attrib res p = reserved res *> colon *> p <* optional comma

-- |a basic numeric expression, using parsec expression builder
compExpr  :: Parser C.Expr
compExpr  =  buildExpressionParser exprOpTable compTerm <?> "expression"

-- |Expression operator precedence table
-- TODO - add parens and commas to the expressions?
-- exprOpTable :: OperatorTable String () Identity C.Expr
exprOpTable =
    [
    [prefix "-" C.Neg, prefix "!" C.Not, prefix "not" C.Not]
    ,[binary "*" C.Mul AssocLeft, binary "/" C.Div AssocLeft, binary "%" C.Mod AssocLeft]
    ,[binary "+" C.Add AssocLeft, binary "-" C.Sub AssocLeft]
    ,[binary "<" C.LT AssocLeft, binary "<=" C.LE AssocLeft, binary ">" C.GT AssocLeft, binary ">=" C.GE AssocLeft]
    ,[binary "==" C.EQ AssocLeft, binary "!=" C.NEQ AssocLeft]
    ,[binary "&&" C.And AssocLeft, binary "and" C.And AssocLeft]
    ,[binary "||" C.Or AssocLeft, binary "or" C.Or AssocLeft]
    ]
  where
    binary name binop assoc = Infix (reservedOp name *> pure (\a b -> C.BinExpr binop a b) <?> "binary operator") assoc
    prefix name unop         = Prefix (reservedOp name *> pure (\a -> C.UnExpr unop a) <?> "unary operator")

-- |parser for a numerical sequence, e.g. [a, b .. c]
-- where a is the start, b is the next element, and c is the stop
numSeqTerm :: Parser C.Expr
numSeqTerm = createSeq <$> number <*> (comma *> number) <*> (symbol ".." *> number) <?> "numerical sequence"
  where
    createSeq a b c = C.NumSeq a (b-a) c

piecewiseTerm :: Parser C.Expr
piecewiseTerm = C.Piecewise <$> (endBy1 ((,) <$> compExpr <*> (colon *> compExpr)) comma)
                            <*> (reserved "default" *> colon *> compExpr)

-- |parse a term - the value on either side of an operator
-- should ODEs be here - as terms or statements?
compTerm :: Parser C.Expr
compTerm =  parens compExpr
            <|> C.Number <$> number
            <|> try (brackets numSeqTerm)
            <|> try (braces piecewiseTerm)
            <|> try (C.Call <$> modLocalIdentifier <*> paramList compExpr)
            <|> C.ValueRef <$> modLocalIdentifier
            <?> "valid term"

{-
TODO - File GHC/Parsec bug

odeDef :: (Double -> C.Expr -> C.CompStmt) -> Parser C.CompStmt
odeDef n = permute (n
            <$$> (attrib "init" number)
            <||> (attrib "delta" compExpr)
            )
            -- <|> ((C.OdeDef <$> (reserved "ode" *> identifier <* reservedOp "=" )) >>= (\n -> braces (odeDef n)))
-}

-- |parse an ode attribute definition
odeDef = permute (C.OdeDef ""
            <$$> (attrib "init" number)
            <||> (attrib "delta" compExpr)
            ) <?> "ode definition"

-- |parse a rre attribute definition
rreDef = permute (C.RreDef ""
            <$$> (attrib "reaction" ((,) <$> identifier <*> (reservedOp "->" *> identifier)))
            <||> (attrib "rate" compExpr)
            ) <?> "rre definition"

-- |parse a value definition
-- e.g., val x = expr
valueDef :: Parser C.ValueDef
valueDef = C.ValueDef   <$> (reserved "val" *> commaSep1 identifier) <*> (reservedOp "=" *> compExpr)

-- |parser for the statements allowed within a component body
compStmt :: Parser C.CompStmt
compStmt =  --C.CompCallDef <$> commaSep1 identifier <*> (reservedOp "=" *> identifier) <*> paramList compExpr
            C.CompValue <$> valueDef
            <|> C.InitValueDef <$> (reserved "init" *> commaSep1 identifier) <*> (reservedOp "=" *> compExpr)
            <|> updateOde <$> (reserved "ode" *> identifier) <*> (reservedOp "=" *> braces odeDef)
            <|> updateRre <$> (reserved "rre" *> identifier) <*> (reservedOp "=" *> braces rreDef)
            <?> "valid component statement"
  where
    updateOde n ode = ode {C.odeName = n}
    updateRre n rre = rre {C.rreName = n}

-- |parser for the component body, a list of statements and return expressions
compBody :: Parser ([C.CompStmt], [C.Expr])
compBody = (,)  <$> many compStmt --compStmt `endBy` lexeme newline --
                <*> (reserved "return" *> paramList compExpr)

-- |parser for defining a component, where either a defintion or module parameter component may follow
compDef :: Parser C.Component
compDef = do
    cName <- reserved "component" *> identifier
    compParse cName
  where
    compParse cName =
        C.ComponentRef <$> pure cName <*> (reservedOp "=" *> modElemIdentifier)
        <|> (uncurry <$> (C.Component <$> pure cName <*> paramList identifier) <*> braces compBody)
        <?> "component definition"

-- |parse the body of a module
moduleBody :: Parser C.ModuleElem
moduleBody =    C.ModuleElemComponent <$> compDef
                <|> C.ModuleElemValue <$> valueDef
                <?> "component or value defintion"

-- |parse a chain/tree of module applications
moduleAppParams :: Parser C.ModuleAppParams
moduleAppParams = C.ModuleAppParams <$> modIdentifier <*> optionMaybe (paramList moduleAppParams)

-- |parse a module, either an entire definition/abstraction or an application
moduleDef :: Parser C.Module
moduleDef = do
    mName <- reserved "module" *> modIdentifier
    modParse mName
  where
    modParse mName =
        C.ModuleApp <$> pure mName <*> (reservedOp "=" *> moduleAppParams)
        <|> C.ModuleAbs <$> pure mName <*> optionMaybe (paramList modIdentifier) <*> braces (many1 moduleBody)
        <?> "module definition"

-- |parse the open directive
moduleOpen :: Parser C.FileOpen
moduleOpen = reserved "open" *> stringLiteral

-- |top level parser for a file
coreTop :: Parser C.Model
coreTop = C.Model <$> (whiteSpace *> many moduleOpen) <*> many1 moduleDef <* eof

-- |parses the string and returns the result if sucessful
-- maybe move into main
-- TODO - switch to bytestring?
coreParse :: FilePath -> String -> MExcept C.Model
coreParse fileName fileData =
    -- do  parseRes <- parseFromFile odeMain fileName
    case parseRes of
        Left err    -> Left ("parse error at " ++ show err)
        Right res   -> Right res
  where
    parseRes = parse coreTop fileName fileData
