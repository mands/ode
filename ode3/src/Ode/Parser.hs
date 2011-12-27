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

module Ode.Parser (
moduleBody
) where

import Control.Applicative
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Perm

import Common.Parser
import Utils.Utils
import qualified Ode.AST as O

-- |parse the body of a module
moduleBody = stmt

stmt :: Parser O.Stmt
stmt =  compDef
        <|> valueDef
        <|> sValueDef
        <|> odeDef
        <|> rreDef
        <?> "component, value or simulation defintion"

-- |parse a value definition
-- e.g., val x = expr
valueDef :: Parser O.Stmt
valueDef = O.Value  <$> (reserved "val" *> commaSep1 valIdentifier) <*> (reservedOp "=" *> compExpr)
                    <*> option [] (reserved "where" *> valBody)
  where
    valBody = braces $ many stmt

-- |parse a sval def
sValueDef :: Parser O.Stmt
sValueDef = O.SValue <$> (reserved "sval" *> commaSep1 valIdentifier) <*> (reservedOp "=" *> singOrList number)


-- |parser for defining a component, where either a defintion or module parameter component may follow
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
    compBody = braces $ many stmt

odeDef :: Parser O.Stmt
odeDef = O.OdeDef <$> (reserved "ode" *> valIdentifier) <*> pure 0.0 <*> (reservedOp "=" *> compExpr)

rreDef :: Parser O.Stmt
rreDef = O.RreDef <$> (reserved "rre" *> braces rreAttribs) <*> (reservedOp "=" *> identifier) <*> (reservedOp "->" *> identifier)
  where
    -- |parse a rre attribute definition
    rreAttribs = attrib "rate" number

-- |parser for the statements allowed within a component body
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


-- |parse a term - the value on either side of an operator
-- should ODEs be here - as terms or statements?
compTerm :: Parser O.Expr
compTerm =  try (parens compExpr)
            <|> O.Number <$> number
            <|> try (O.Boolean <$> boolean)
            <|> try (time *> pure O.Time)
            <|> try (unit *> pure O.Unit)
            <|> try (brackets numSeqTerm)
            <|> try (braces piecewiseTerm)
            <|> try (O.Call <$> modLocalIdentifier <*> paramList compExpr)
            <|> O.ValueRef <$> modLocalIdentifier
            <|> O.Tuple <$> tuple compExpr
            <?> "valid term"

piecewiseTerm :: Parser O.Expr
piecewiseTerm = O.Piecewise <$> (endBy1 ((,) <$> compExpr <*> (colon *> compExpr)) comma)
                            <*> (reserved "default" *> colon *> compExpr)

-- | parser for a numerical sequence, e.g. [a, b .. c]
-- where a is the start, b is the next element, and c is the stop
numSeqTerm :: Parser O.Expr
numSeqTerm = createSeq <$> number <*> (comma *> number) <*> (symbol ".." *> number) <?> "numerical sequence"
  where
    createSeq a b c = O.NumSeq a b c

-- |a basic numeric expression, using parsec expression builder
compExpr  :: Parser O.Expr
compExpr  =  buildExpressionParser exprOpTable compTerm <?> "expression"

-- |Expression operator precedence table, based on C
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

-- |parses a module element reference, e.g. A.x
modElemIdentifier :: Parser O.ModLocalId
modElemIdentifier  = lexeme (O.ModId <$> upperIdentifier <*> (char '.' *> identifier))

-- |parse either a local or module id e.g. A.x or x
modLocalIdentifier :: Parser O.ModLocalId
modLocalIdentifier =    try modElemIdentifier
                        <|> O.LocalId <$> identifier <?> "local or module identifier"

-- | value identifier, allows use of don't care vals
valIdentifier :: Parser O.ValId
valIdentifier = reservedOp "_" *> pure O.DontCare
                <|> O.ValId <$> identifier <*> optionMaybe (braces unitAttrib)
                <?> "value identifier"
  where
    unitAttrib = attrib "unit" (many alphaNum)















