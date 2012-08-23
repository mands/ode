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
--
-----------------------------------------------------------------------------

module Lang.Ode.Parser (
odeStmt
) where

import Control.Applicative
import Control.Monad.Identity

import Text.Parsec hiding (many, optional, (<|>))
--import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Perm
import qualified Data.Map as Map

import Lang.Common.Parser
import qualified Lang.Common.Ops as Ops
import qualified Lang.Common.AST as CA
import Utils.Utils
import qualified Lang.Ode.AST as O


-- Useful Lexical Combinators ------------------------------------------------------------------------------------------

-- | parses a module element reference, e.g. A.x
modElemIdentifier :: Parser O.RefId
modElemIdentifier  = O.ModId <$> upperIdentifier <*> (char '.' *> identifier)

-- | parse either a local or module id e.g. A.x or x
modLocalIdentifier :: Parser O.RefId
modLocalIdentifier =    try modElemIdentifier
                        <|> O.LocalId <$> identifier <?> "local or module identifier"

-- | parse either a local or module id e.g. A.x or x
typeIdentifier :: Parser O.RefId
typeIdentifier =    try (O.ModId <$> upperIdentifier <*> (char '.' *> upperIdentifier))
                    <|> O.LocalId <$> upperIdentifier <?> "local or module type identifier"

-- | value identifier, allows use of don't care vals
valIdentifier :: Parser O.BindId
valIdentifier = reservedOp "_" *> pure O.DontCare
                <|> O.BindId <$> identifier
                <?> "value identifier"

-- | Wrapper around our default attribute notation
attribDef p = braces (permute p)

-- | Combinator for a single attrib
singAttrib res p = braces (reserved res *> colon *> p)

-- | a parameterised single attribute parser for a given attribute identifier
-- TODO - fix the comma separated list of attribute, commaSep?
-- attrib :: String -> Parser String
attrib res p = reserved res *> colon *> p <* optional comma

-- | tuple, ensures at least two values, comma separated
tuple :: Parser a -> Parser [a]
tuple p = parens $ (:) <$> (p <* comma) <*> commaSep1 p

-- | a named tuple (fixed record), >= 1 values allowed
namedTuple :: Parser a -> Parser [(O.SrcId, a)]
namedTuple p = braces $ commaSep1 ((,) <$> identifier <*> (colon *> p))


-- | used to parse a single element by itself, a, or contained eithin a comma-sep list, (a,...)
singOrList :: Parser a -> Parser [a]
singOrList p = try ((\p -> p:[]) <$> (p))
                <|> paramList p
                <?> "single element or list"


-- Main Ode Parser -----------------------------------------------------------------------------------------------------

-- | main parser into the Ode lang, returns a list of ODE statments
-- these can be, comp/value defs, or units/type defs
odeStmt :: Parser O.OdeStmt
odeStmt =   O.ImportStmt <$> importCmd
            <|> O.ExprStmt <$> exprStmt -- main lang
            <|> quantityDef     -- units support
            <|> unitDef
            <|> convDef
            <|> typeDef
            <?> "import, expression, or unit defintion"


typeDef :: Parser O.OdeStmt
typeDef = O.TypeStmt <$> (reserved "type" *> upperIdentifier)


-- Units Parser --------------------------------------------------------------------------------------------------------

-- | Parses a quantity "alias" for a given dimension
quantityDef :: Parser O.OdeStmt
quantityDef = O.QuantityStmt <$> (reserved "quantity" *> identifier) <*> (reservedOp "=" *> dimTerm)

-- | Parse a DimVec term, like "Dim LT-2"
dimTerm :: Parser CA.DimVec
dimTerm = reserved "dim" *> lexeme parseDims
  where
    parseDims :: Parser CA.DimVec
    parseDims = permute (CA.DimVec <$?> (0, parseDim 'L')
                                <|?> (0, parseDim 'M')
                                <|?> (0, parseDim 'T')
                                <|?> (0, parseDim 'I')
                                <|?> (0, parseDim 'O')
                                <|?> (0, parseDim 'J')
                                <|?> (0, parseDim 'N')
                        )
    -- parseDim dim = char dim *> option 1 (reservedOp "^" *> integer <* char '.')
    parseDim dim = char dim *> option 1 (reservedOp "^" *> integer) <* optional (char '.')

-- TODO - should this be a lexeme ??
unitIdentifier :: Parser CA.UnitList
unitIdentifier = (sepBy1 parseSingUnit $ char '.')
  where
    parseSingUnit = (,) <$> alphaIdentifier <*> option 1 (reservedOp "^" *> integer)

unitAttrib :: Parser CA.UnitList
unitAttrib = singAttrib "unit" unitIdentifier


-- | Parses an avaiable unit definition for a given dimension, with optional alias
unitDef :: Parser O.OdeStmt
unitDef = do
    uName <- reserved "unit" *> alphaIdentifier
    unit <- attribDef unitDefAttrib
    return $ unit { O.uName = uName }
  where
    unitDefAttrib = O.UnitStmt ""   <$$> attrib "dim" (Just <$> oneOf "LMTIOJN")
                                    <|?> (Nothing, attrib "alias" (Just <$> identifier))
                                    <||> attrib "SI" boolean -- <?> "unit definition"


-- | Parses a conversion defintion stmt for 2 units within a given dimension
convDef :: Parser O.OdeStmt
convDef = reserved "conversion" *> attribDef (O.ConvDefStmt <$$> attrib "from" alphaIdentifier
                                                            <||> attrib "to" alphaIdentifier
                                                            <||> attrib "factor" convExpr) <?> "conversion definition"


-- | parse a term - the value on either side of an operator
convTerm :: Parser CA.CExpr
convTerm =  try (parens convExpr)
            <|> CA.CNum <$> number
            <|> symbol "x" *> pure CA.CFromId
            <?> "conversion term"

-- | restricted numeric expression for conversion factors only
convExpr  :: Parser CA.CExpr
convExpr  =  buildExpressionParser convExprOpTable convTerm <?> "conversion expression"

convExprOpTable =
    [
    [binary "*" CA.CMul, binary "/" CA.CDiv]
    ,[binary "+" CA.CAdd, binary "-" CA.CSub]
    ]
  where
    binary name binop = Infix (reservedOp name *> pure (\a b -> CA.CExpr binop a b) <?> "binary operator") AssocLeft

-- Ode Expression ------------------------------------------------------------------------------------------------------

-- | main parser into the Ode lang, returns a list of ODE statments
-- these can be, comp/value defs, or units/type defs
exprStmt :: Parser O.Stmt
exprStmt    = compDef
            <|> valueDef
            <|> sValueDef
            <|> odeDef
            <|> rreDef
            <?> "component, value or simulation defintion"

-- |parse a sval/initial condition def
sValueDef :: Parser O.Stmt
sValueDef = O.SValue <$> (reserved "init" *> commaSep1 identifier) <*> (reservedOp "=" *> compExpr)

-- | parse a value definition
-- e.g., val x = expr
valueDef :: Parser O.Stmt
valueDef = O.Value  <$> (reserved "val" *> commaSep1 valIdentifier <* reservedOp "=")
                    <*> (try singVal <|> blockStmt)
  where
    singVal = (,) <$> pure [] <*> compExpr

-- | parsers a independent block of statements, i.e. { a = 1, b = 2, ..., return z }
blockStmt :: Parser ([O.Stmt], O.Expr)
blockStmt = braces $ (,) <$> (option [] (many exprStmt)) <*> (reserved "return" *> compExpr)

-- | parser for defining a component
compDef :: Parser O.Stmt
compDef = O.Component   <$> (reserved "component" *> identifier)
                        <*> singOrList valIdentifier
                        <*> blockStmt
                        <?> "component definition"

odeDef :: Parser O.Stmt
odeDef = do
    (initRef, deltaName) <- reserved "ode" *> attribDef odeAttribs
    expr <- reservedOp "=" *> compExpr
    return $ O.OdeDef initRef deltaName expr
  where
    odeAttribs  = (,)   <$$> attrib "init" identifier
                        <|?> (O.DontCare, attrib "delta" valIdentifier)

rreDef :: Parser O.Stmt
rreDef = O.RreDef <$> (reserved "rre" *> braces rreAttribs) <*> (reservedOp "=" *> identifier) <*> (reservedOp "->" *> identifier)
  where
    -- | parse a rre attribute definition
    rreAttribs = attrib "rate" number


-- Ode Terms -----------------------------------------------------------------------------------------------------------

-- | high level wrapper around compTerm', allows for type/unit-casting a term
compTerm :: Parser O.Expr
compTerm = do
    e <- compTerm'
    option e $ exprAttrib e
  where
    exprAttrib e =  try (O.ConvCast <$> pure e <*> unitAttrib)
                    <|> try (O.WrapType <$> pure e <*> singAttrib "wrap" typeIdentifier)
                    <|> try (O.UnwrapType <$> pure e <*> singAttrib "unwrap" typeIdentifier)

-- | parse a term - the value on either side of an operator
compTerm' :: Parser O.Expr
compTerm' = try (parens compExpr)
            <|> O.Number <$> number <*> optionMaybe (try unitAttrib)
            <|> O.Boolean <$> boolean
            <|> (reserved "time" *> pure O.Time) -- put into Environment module instead ??
            <|> (reserved "None" *> pure O.None)
            <|> reserved "piecewise" *> piecewiseTerm
            <|> try (O.Call <$> modLocalIdentifier <*> paramList compExpr)
            <|> O.ValueRef <$> modLocalIdentifier <*> optionMaybe (reservedOp "#" *> identifier)
            <|> O.Tuple <$> tuple compExpr
            <|> O.Record <$> namedTuple compExpr
            -- <|> brackets numSeqTerm
            <?> "valid term"

piecewiseTerm :: Parser O.Expr
piecewiseTerm = braces (O.Piecewise <$> endBy1 ((,) <$> compExpr <*> (colon *> compExpr)) comma
                                    <*> (reserved "default" *> colon *> compExpr))

-- | parser for a numerical sequence, e.g. [a, b .. c]
-- where a is the start, b is the next element, and c is the stop
numSeqTerm :: Parser O.Expr
numSeqTerm = createSeq <$> number <*> (comma *> number) <*> (symbol ".." *> number) <?> "numerical sequence"
  where
    createSeq a b c = O.NumSeq a b c

-- | a basic numeric expression, using parsec expression builder
compExpr  :: Parser O.Expr
compExpr  =  buildExpressionParser exprOpTable compTerm <?> "expression"

-- | Expression operator precedence table, based on C
-- TODO - add parens and commas to the expressions?
exprOpTable :: OperatorTable String () Identity O.Expr
exprOpTable =
    [
    [prefix "-" Ops.Neg, prefix "!" Ops.Not, prefix "not" Ops.Not]
    ,[binary "*" Ops.Mul AssocLeft, binary "/" Ops.Div AssocLeft, binary "%" Ops.Mod AssocLeft]
    ,[binary "+" Ops.Add AssocLeft, binary "-" Ops.Sub AssocLeft]
    ,[binary "<" Ops.LT AssocLeft, binary "<=" Ops.LE AssocLeft, binary ">" Ops.GT AssocLeft, binary ">=" Ops.GE AssocLeft]
    ,[binary "==" Ops.EQ AssocLeft, binary "!=" Ops.NEQ AssocLeft]
    ,[binary "&&" Ops.And AssocLeft, binary "and" Ops.And AssocLeft]
    ,[binary "||" Ops.Or AssocLeft, binary "or" Ops.Or AssocLeft]
    ]
  where
    binary name binop assoc = Infix (reservedOp name *> pure (\a b -> O.Op (Ops.BasicOp binop) [a, b]) <?> "binary operator") assoc
    prefix name unop         = Prefix (reservedOp name *> pure (\a -> O.Op (Ops.BasicOp unop) [a]) <?> "unary operator")

