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
import qualified Lang.Common.AST as CA
import Utils.Utils
import qualified Lang.Ode.AST as O


-- Useful Lexical Combinators ------------------------------------------------------------------------------------------

-- | parses a module element reference, e.g. A.x
modElemIdentifier :: Parser O.ModLocalId
modElemIdentifier  = lexeme (O.ModId <$> upperIdentifier <*> (char '.' *> identifier))

-- | parse either a local or module id e.g. A.x or x
modLocalIdentifier :: Parser O.ModLocalId
modLocalIdentifier =    try modElemIdentifier
                        <|> O.LocalId <$> identifier <?> "local or module identifier"

-- | value identifier, allows use of don't care vals
valIdentifier :: Parser O.ValId
valIdentifier = reservedOp "_" *> pure O.DontCare
                <|> O.ValId <$> identifier
                <?> "value identifier"

-- | Wrapper around our default attribute notation
attribDef p = braces (permute p)

-- | a parameterised single attribute parser for a given attribute identifier
-- TODO - fix the comma separated list of attribute, commaSep?
-- attrib :: String -> Parser String
attrib res p = reserved res *> colon *> p <* optional comma

-- | tuple, requires at least two values, comma separated
tuple :: Parser a -> Parser [a]
tuple p = parens $ (:) <$> (p <* comma) <*> commaSep1 p

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
            <?> "import, expression, or unit defintion"

-- | Parses a quantity "alias" for a given dimension
quantityDef :: Parser O.OdeStmt
quantityDef = O.QuantityStmt <$> (reserved "quantity" *> identifier) <*> (reservedOp "=" *> dimTerm)

-- | Parse a DimVec term, like "Dim LT-2"
dimTerm :: Parser CA.DimVec
dimTerm = reserved "dim" *> parseDims
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
    parseDim dim = char dim *> option 1 integer

unitIdentifier :: Parser CA.SrcUnit
unitIdentifier = (sepBy parseSingUnit $ char '.')
  where
    parseSingUnit = (,) <$> identifier <*> option 1 integer

-- | Parses an avaiable unit definition for a given dimension, with optional alias
unitDef :: Parser O.OdeStmt
unitDef =   try baseDef
            <|> derivedDef
  where
    baseDef = do
        uName <- reserved "unit" *> identifier
        unit <- attribDef singUnitAttrib
        return $ unit { O.uName = [(uName,1)] }
    derivedDef = do
        uName <- reserved "unit" *> unitIdentifier
        mAlias <- option Nothing $ braces (attrib "alias" (Just <$> identifier))
        return $ O.UnitStmt uName Nothing mAlias False

    singUnitAttrib = O.UnitStmt [] <$$> attrib "dim" (Just <$> oneOf "LMTIOJN")
                                   <|?> (Nothing, attrib "alias" (Just <$> identifier))
                                   <||> attrib "SI" boolean -- <?> "unit definition"

-- | Parses a conversion defintion stmt for 2 units within a given dimension
convDef :: Parser O.OdeStmt
convDef = reserved "conversion" *> attribDef (O.ConvDefStmt <$$> attrib "from" unitIdentifier
                                                            <||> attrib "to" unitIdentifier
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

-- | parse a value definition
-- e.g., val x = expr
valueDef :: Parser O.Stmt
valueDef = O.Value  <$> (reserved "val" *> commaSep1 valIdentifier) <*> (reservedOp "=" *> compExpr)
                    <*> option [] (reserved "where" *> valBody)
  where
    valBody = braces $ many exprStmt

-- |parse a sval def
sValueDef :: Parser O.Stmt
sValueDef = O.SValue <$> (reserved "sval" *> commaSep1 valIdentifier) <*> (reservedOp "=" *> singOrList number)


-- | parser for defining a component, where either a defintion or module parameter component may follow
compDef :: Parser O.Stmt
compDef = do
    cName <- reserved "component" *> identifier
    compParse cName
  where
    compParse cName = O.Component <$> pure cName <*> singOrList valIdentifier <*>
            (reservedOp "=>" *> compExpr) <*> option [] (reserved "where" *> compBody)
        -- O.ComponentRef <$> pure cName <*> (reservedOp "=" *> modElemIdentifier)
        -- <|>
        <?> "component definition"

    -- | parser for the component body, a list of statements
    compBody = braces $ many exprStmt

odeDef :: Parser O.Stmt
odeDef = O.OdeDef <$> (reserved "ode" *> valIdentifier) <*> pure 0.0 <*> (reservedOp "=" *> compExpr)

rreDef :: Parser O.Stmt
rreDef = O.RreDef <$> (reserved "rre" *> braces rreAttribs) <*> (reservedOp "=" *> identifier) <*> (reservedOp "->" *> identifier)
  where
    -- | parse a rre attribute definition
    rreAttribs = attrib "rate" number


-- | parser for the statements allowed within a component body
--compStmt :: Parser O.CompStmt
--compStmt =  --O.CompCallDef <$> commaSep1 identifier <*> (reservedOp "=" *> identifier) <*> paramList compExpr
--            O.CompComp <$> compDef
--            <|> O.CompValue <$> valueDef
--            <|> O.InitValueDef <$> (reserved "init" *> commaSep1 identifier) <*> (reservedOp "=" *> compExpr)
--            <|> updateOde <$> (reserved "ode" *> identifier) <*> (reservedOp "=" *> braces odeDef)
--            <|> updateRre <$> (reserved "rre" *> identifier) <*> (reservedOp "=" *> braces rreDef)
--            <?> "valid component statement"
--  where
--    updateOde n ode = ode {O.odeName = n}
--    updateRre n rre = rre {O.rreName = n}


--rreDef = permute (O.RreDef ""
--            <$$> (attrib "reaction" ((,) <$> identifier <*> (reservedOp "->" *> identifier)))
--            <||> (attrib "rate" compExpr)
--            ) <?> "rre definition"

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
--odeDef = permute (O.OdeDef ""
--            <$$> (attrib "init" number)
--            <||> (attrib "delta" compExpr)
--            ) <?> "ode definition"


-- Ode Terms -----------------------------------------------------------------------------------------------------------

-- | time term, is a special identifier
time :: Parser ()
time = reserved "time" *> pure ()

-- | unit term, is a special identifier
unit :: Parser ()
unit = reservedOp "()" *> pure ()

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

-- | parse a term - the value on either side of an operator
-- should ODEs be here - as terms or statements?
compTerm :: Parser O.Expr
compTerm = -- try unitExpr
            unitT (try (parens compExpr))
            <|> unitT (try (O.Number <$> number))
            <|> try (O.Boolean <$> boolean)
            <|> unitT (try (time *> pure O.Time))
            <|> try (unit *> pure O.Unit)
            -- <|> try (brackets numSeqTerm)
            <|> unitT (try (braces piecewiseTerm))
            <|> unitT (try (O.Call <$> modLocalIdentifier <*> paramList compExpr))
            -- <|> try convertCastStmt
            <|> unitT (O.ValueRef <$> modLocalIdentifier)
            <|> O.Tuple <$> tuple compExpr
            <?> "valid term"

-- | parse a term then check for an, optional, trailing unit cast
unitT :: Parser O.Expr -> Parser O.Expr
unitT p = do
    e1 <- p
    option e1 $ O.ConvCast <$> pure e1 <*> braces (attrib "unit" unitIdentifier)


piecewiseTerm :: Parser O.Expr
piecewiseTerm = O.Piecewise <$> (endBy1 ((,) <$> compExpr <*> (colon *> compExpr)) comma)
                            <*> (reserved "default" *> colon *> compExpr)

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

