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

module Parser.Ode (
odeStmt
) where

import Control.Applicative
import Control.Monad.Identity

import Text.Parsec hiding (many, optional, (<|>))
--import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Perm
import qualified Data.Map as Map

import Parser.Common
import qualified AST.Common as AC
import Utils.Utils
import qualified AST.Ode as AO


-- Useful Lexical Combinators ------------------------------------------------------------------------------------------

-- | parses a module element reference, e.g. A.x
modElemIdentifier :: Parser AO.RefId
modElemIdentifier  = AO.ModId <$> upperIdentifier <*> (char '.' *> identifier)

-- | parse either a local or module id e.g. A.x or x
modLocalIdentifier :: Parser AO.RefId
modLocalIdentifier =    try modElemIdentifier
                        <|> AO.LocalId <$> identifier <?> "local or module identifier"

-- | parse either a local or module id e.g. A.x or x
typeIdentifier :: Parser AO.RefId
typeIdentifier =    try (AO.ModId <$> upperIdentifier <*> (char '.' *> upperIdentifier))
                    <|> AO.LocalId <$> upperIdentifier <?> "local or module type identifier"

-- | value identifier, allows use of don't care vals
valIdentifier :: Parser AO.BindId
valIdentifier = reservedOp "_" *> pure AO.DontCare
                <|> AO.BindId <$> identifier
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
namedTuple :: Parser a -> Parser [(AO.SrcId, a)]
namedTuple p = braces $ commaSep1 ((,) <$> identifier <*> (colon *> p))


-- | used to parse a single element by itself, a, or contained eithin a comma-sep list, (a,...)
singOrList :: Parser a -> Parser [a]
singOrList p = try ((\p -> p:[]) <$> (p))
                <|> paramList p
                <?> "single element or list"


-- Main Ode Parser -----------------------------------------------------------------------------------------------------

-- | main parser into the Ode lang, returns a list of ODE statments
-- these can be, comp/value defs, or units/type defs
odeStmt :: Parser AO.OdeStmt
odeStmt =   AO.ImportStmt <$> importCmd
            <|> AO.ExprStmt <$> exprStmt -- main lang
            <|> quantityDef     -- units support
            <|> unitDef
            <|> convDef
            <|> typeDef
            <?> "import, expression, or unit defintion"


typeDef :: Parser AO.OdeStmt
typeDef = AO.TypeStmt <$> (reserved "type" *> upperIdentifier)


-- Units Parser --------------------------------------------------------------------------------------------------------

-- | Parses a quantity "alias" for a given dimension
quantityDef :: Parser AO.OdeStmt
quantityDef = AO.QuantityStmt <$> (reserved "quantity" *> identifier) <*> (reservedOp "=" *> dimTerm)

-- | Parse a DimVec term, like "Dim LT-2"
dimTerm :: Parser AC.DimVec
dimTerm = reserved "dim" *> lexeme parseDims
  where
    parseDims :: Parser AC.DimVec
    parseDims = permute (AC.DimVec <$?> (0, parseDim 'L')
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
unitIdentifier :: Parser AC.UnitList
unitIdentifier = (sepBy1 parseSingUnit $ char '.')
  where
    parseSingUnit = (,) <$> alphaIdentifier <*> option 1 (reservedOp "^" *> integer)

unitAttrib :: Parser AC.UnitList
unitAttrib = singAttrib "unit" unitIdentifier


-- | Parses an avaiable unit definition for a given dimension, with optional alias
unitDef :: Parser AO.OdeStmt
unitDef = do
    uName <- reserved "unit" *> alphaIdentifier
    unit <- attribDef unitDefAttrib
    return $ unit { AO.uName = uName }
  where
    unitDefAttrib = AO.UnitStmt ""  <$$> attrib "dim" (Just <$> oneOf "LMTIOJN")
                                    <|?> (Nothing, attrib "alias" (Just <$> identifier))
                                    <||> attrib "SI" boolean -- <?> "unit definition"


-- | Parses a conversion defintion stmt for 2 units within a given dimension
convDef :: Parser AO.OdeStmt
convDef = reserved "conversion" *> attribDef (AO.ConvDefStmt    <$$> attrib "from" alphaIdentifier
                                                                <||> attrib "to" alphaIdentifier
                                                                <||> attrib "factor" convExpr) <?> "conversion definition"


-- | parse a term - the value on either side of an operator
convTerm :: Parser AC.CExpr
convTerm =  try (parens convExpr)
            <|> AC.CNum <$> number
            <|> symbol "x" *> pure AC.CFromId
            <?> "conversion term"

-- | restricted numeric expression for conversion factors only
convExpr  :: Parser AC.CExpr
convExpr  =  buildExpressionParser convExprOpTable convTerm <?> "conversion expression"

convExprOpTable =
    [
    [binary "*" AC.CMul, binary "/" AC.CDiv]
    ,[binary "+" AC.CAdd, binary "-" AC.CSub]
    ]
  where
    binary name binop = Infix (reservedOp name *> pure (\a b -> AC.CExpr binop a b) <?> "binary operator") AssocLeft

-- Ode Expression ------------------------------------------------------------------------------------------------------

-- | main parser into the Ode lang, returns a list of ODE statments
-- these can be, comp/value defs, or units/type defs
exprStmt :: Parser AO.Stmt
exprStmt    = compDef
            <|> valueDef
            <|> sValueDef
            <|> odeDef
            <|> sdeDef
            <|> rreDef
            <?> "component, value or simulation defintion"

-- |parse a sval/initial condition def
-- TODO - do we allowed blockStmt for inits?
sValueDef :: Parser AO.Stmt
sValueDef = AO.SValue <$> (reserved "init" *> identifier) <*> (reservedOp "=" *> compExpr)

-- | parse a value definition
-- e.g., val x = expr
valueDef :: Parser AO.Stmt
valueDef = AO.Value  <$> (reserved "val" *> commaSep1 valIdentifier <* reservedOp "=")
                    <*> (try singVal <|> blockStmt)
  where
    singVal = (,) <$> pure [] <*> compExpr

-- | parsers a independent block of statements, i.e. { a = 1, b = 2, ..., return z }
blockStmt :: Parser ([AO.Stmt], AO.Expr)
blockStmt = braces $ (,) <$> (option [] (many exprStmt)) <*> (reserved "return" *> compExpr)

-- | parser for defining a component
compDef :: Parser AO.Stmt
compDef = AO.Component  <$> (reserved "component" *> identifier)
                        <*> singOrList valIdentifier
                        <*> blockStmt
                        <?> "component definition"

odeDef :: Parser AO.Stmt
odeDef = do
    (initRef, deltaName) <- reserved "ode" *> attribDef odeAttribs
    expr <- reservedOp "=" *> compExpr
    return $ AO.OdeDef initRef deltaName expr
  where
    odeAttribs  = (,)   <$$> attrib "init" identifier
                        <|?> (AO.DontCare, attrib "delta" valIdentifier)

sdeDef :: Parser AO.Stmt
sdeDef = do
    sdeExpr <- reserved "sde" *> attribDef sdeAttribs
    expr <- reservedOp "=" *> compExpr
    return $ sdeExpr expr
  where
    sdeAttribs  = AO.SdeDef     <$$> attrib "init" identifier
                                <|?> (AO.DontCare, attrib "delta" valIdentifier)
                                <||> attrib "weiner" compExpr

rreDef :: Parser AO.Stmt
rreDef = AO.RreDef <$> (reserved "rre" *> braces rreAttribs) <*> (reservedOp "=" *> speciesList) <*> (reservedOp "->" *> speciesList)
  where
    -- | parse a rre attribute definition
    -- TODO - this could/should be an expression
    rreAttribs = attrib "rate" number
    rreSpecies = (,) <$> option 1 (natural <* symbol  ".") <*> identifier
    speciesList = sepBy1 rreSpecies (reservedOp "+")


-- Ode Terms -----------------------------------------------------------------------------------------------------------

-- | high level wrapper around compTerm', allows for type/unit-casting a term
compTerm :: Parser AO.Expr
compTerm = do
    e <- compTerm'
    option e $ exprAttrib e
  where
    exprAttrib e =  try (AO.ConvCast <$> pure e <*> unitAttrib)
                    <|> try (AO.WrapType <$> pure e <*> singAttrib "wrap" typeIdentifier)
                    <|> try (AO.UnwrapType <$> pure e <*> singAttrib "unwrap" typeIdentifier)

-- | parse a term - the value on either side of an operator
-- these should not be stateful, however we could call a component that in turn creates state/init values
compTerm' :: Parser AO.Expr
compTerm' = try (parens compExpr)
            <|> AO.Number <$> number <*> optionMaybe (try unitAttrib)
            <|> AO.Boolean <$> boolean
            <|> (reserved "time" *> pure AO.Time) -- TODO - put into Environment module instead ??
            <|> (reserved "None" *> pure AO.None)
            <|> reserved "piecewise" *> piecewiseTerm
            <|> try (hardcodedOps)
            <|> try (AO.Op <$> builtinOpParser <*> paramList compExpr)
            <|> try (AO.Call <$> modLocalIdentifier <*> paramList compExpr)
            <|> AO.ValueRef <$> modLocalIdentifier <*> optionMaybe (reservedOp "#" *> identifier)
            <|> AO.Tuple <$> tuple compExpr
            <|> AO.Record <$> namedTuple compExpr
            -- <|> brackets numSeqTerm
            <?> "valid term"

piecewiseTerm :: Parser AO.Expr
piecewiseTerm = braces (AO.Piecewise <$> endBy1 ((,) <$> compExpr <*> (colon *> compExpr)) comma
                                    <*> (reserved "default" *> colon *> compExpr))

-- | parser for a numerical sequence, e.g. [a, b .. c]
-- where a is the start, b is the next element, and c is the stop
numSeqTerm :: Parser AO.Expr
numSeqTerm = createSeq <$> number <*> (comma *> number) <*> (symbol ".." *> number) <?> "numerical sequence"
  where
    createSeq a b c = AO.NumSeq a b c

-- | a basic numeric expression, using parsec expression builder
-- i.e. the value on the rhs of an expression
compExpr  :: Parser AO.Expr
compExpr  =  buildExpressionParser exprOpTable compTerm <?> "expression"

-- | Expression operator precedence table, based on C
-- TODO - add parens and commas to the expressions?
exprOpTable :: OperatorTable String () Identity AO.Expr
exprOpTable =
    [
    [prefix "-" AC.Neg, prefix "!" AC.Not, prefix "not" AC.Not]
    ,[binary "*" AC.Mul AssocLeft, binary "/" AC.Div AssocLeft] -- , binary "%" AC.Mod AssocLeft]
    ,[binary "+" AC.Add AssocLeft, binary "-" AC.Sub AssocLeft]
    ,[binary "<=" AC.LE AssocLeft, binary ">=" AC.GE AssocLeft, binary "<" AC.LT AssocLeft, binary ">" AC.GT AssocLeft]
    ,[binary "==" AC.EQ AssocLeft, binary "!=" AC.NEQ AssocLeft]
    ,[binary "&&" AC.And AssocLeft, binary "and" AC.And AssocLeft]
    ,[binary "||" AC.Or AssocLeft, binary "or" AC.Or AssocLeft]
    ]
  where
    binary name binop assoc = Infix (reservedOp name *> pure (\a b -> AO.Op (AC.BasicOp binop) [a, b]) <?> "binary operator") assoc
    prefix name unop         = Prefix (reservedOp name *> pure (\a -> AO.Op (AC.BasicOp unop) [a]) <?> "unary operator")


-- Ode Builtin Operators -----------------------------------------------------------------------------------------------

builtinOpParser :: Parser AC.Op
builtinOpParser =   reserved "sin"      *> pure (AC.MathOp AC.Sin)
                <|> reserved "cos"      *> pure (AC.MathOp AC.Cos)
                <|> reserved "tan"      *> pure (AC.MathOp AC.Tan)
                -- <|> reserved "sincos"   *> pure (AC.MathOp AC.SinCos) -- removed support for now
                <|> reserved "asin"     *> pure (AC.MathOp AC.ASin)
                <|> reserved "acos"     *> pure (AC.MathOp AC.ACos)
                <|> reserved "atan"     *> pure (AC.MathOp AC.ATan)
                <|> reserved "atan2"    *> pure (AC.MathOp AC.ATan2)
                <|> reserved "exp"      *> pure (AC.MathOp AC.Exp)
                <|> reserved "exp2"     *> pure (AC.MathOp AC.Exp2)
                <|> reserved "exp10"    *> pure (AC.MathOp AC.Exp10)
                <|> reserved "pow10"    *> pure (AC.MathOp AC.Pow10)
                <|> reserved "log"      *> pure (AC.MathOp AC.Log)
                <|> reserved "log2"     *> pure (AC.MathOp AC.Log2)
                <|> reserved "log10"    *> pure (AC.MathOp AC.Log10)
                <|> reserved "logb"     *> pure (AC.MathOp AC.LogB)
                <|> reserved "pow"      *> pure (AC.MathOp AC.Pow)
                <|> reserved "sqrt"     *> pure (AC.MathOp AC.Sqrt)
                <|> reserved "cbrt"     *> pure (AC.MathOp AC.Cbrt)
                <|> reserved "hypot"    *> pure (AC.MathOp AC.Hypot)
                <|> reserved "expm1"    *> pure (AC.MathOp AC.ExpM1)
                <|> reserved "log1p"    *> pure (AC.MathOp AC.Log1P)
                <|> reserved "sinh"     *> pure (AC.MathOp AC.SinH)
                <|> reserved "cosh"     *> pure (AC.MathOp AC.CosH)
                <|> reserved "tanh"     *> pure (AC.MathOp AC.TanH)
                <|> reserved "asinh"    *> pure (AC.MathOp AC.ASinH)
                <|> reserved "acosh"    *> pure (AC.MathOp AC.ACosH)
                <|> reserved "atanh"    *> pure (AC.MathOp AC.ATanH)
                <|> reserved "erf"      *> pure (AC.MathOp AC.Erf)
                <|> reserved "erfc"     *> pure (AC.MathOp AC.ErfC)
                <|> reserved "lgamma"   *> pure (AC.MathOp AC.LGamma)
                <|> reserved "gamma"    *> pure (AC.MathOp AC.TGamma)
                <|> reserved "tgamma"   *> pure (AC.MathOp AC.TGamma)
                -- basic fp ops
                <|> reserved "abs"      *> pure (AC.MathOp AC.FAbs)
                <|> reserved "floor"    *> pure (AC.MathOp AC.Floor)
                <|> reserved "ceil"     *> pure (AC.MathOp AC.Ceil)
                <|> reserved "round"    *> pure (AC.MathOp AC.Round)


-- bit of a hack to handle certain op calls that are not supported explciity within the language semantics,
-- for instace, ops with integer (or string) params
hardcodedOps :: Parser AO.Expr
hardcodedOps =  upow
                <|> uroot
  where
    -- upow and uroot handle +ve integers only
    upow = do
        (e, pow) <- reserved "upow" *> parens ((,) <$> compExpr <*> (comma *> natural))
        return $ AO.Op (AC.OtherOp (AC.UPow pow)) [e]
    uroot = do
        (e, pow) <- reserved "uroot" *> parens ((,) <$> compExpr <*> (comma *> natural))
        return $ AO.Op (AC.OtherOp (AC.URoot pow)) [e]

