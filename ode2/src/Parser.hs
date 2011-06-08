-----------------------------------------------------------------------------
--
-- Module      :  Parser
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Parses an input string into ModelAST using Parsec library
--
-----------------------------------------------------------------------------

module Parser (
    odeParse
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Perm
import Text.ParserCombinators.Parsec.Language( javaStyle )
import Data.Char( digitToInt )
import Data.Map as Map

import Utilities
import qualified AST as A
import qualified ModelAST as M

odeLangDef = javaStyle
    {
        -- add more later
        P.reservedNames = ["simulate", "component", "piecewise", "init", "delta", "weiner"
                          , "return", "default"],
        -- unary ops and relational ops?
        P.reservedOpNames = ["*","/","+","-", "=", ">", ">=", "<", "<=", "==", "and", "or"]
    }

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser odeLangDef

-- For efficiency, we will bind all the used lexical parsers at toplevel.
whiteSpace  = P.whiteSpace lexer
lexeme      = P.lexeme lexer
symbol      = P.symbol lexer
natural     = P.natural lexer
integer     = P.integer lexer
float'      = P.float lexer
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

-- override float parser
float :: Parser Double
float =
    try (float')
    <|> (do { i <- integer; return (fromIntegral i)})

-- | a basic numeric expression
expr  :: Parser M.Expr
expr  =  buildExpressionParser table term <?> "expression"

table :: OperatorTable Char () M.Expr
table =
    [[binary "*" A.Mul AssocLeft, binary "/" A.Div AssocLeft]
    ,[binary "+" A.Add AssocLeft, binary "-" A.Sub AssocLeft]
    ]
  where
    binary name binop assoc = Infix (do { reservedOp name; return (\a b -> M.BinExpr a binop b) } <?> "operator") assoc
    prefix name fun         = Prefix (do { reservedOp name; return fun })

-- | value on either side of an operator
term :: Parser M.Expr
term =
    parens expr
    -- <|> (do { f <- float; return (M.Number f) })
    <|> (float >>= (\f -> return (M.Number f)))
    <|> (do
            reserved "piecewise"
            (cases, e) <- braces (caseAttribs)
            return (M.CaseExp cases e)
        )
{-
    <|> (do reserved "ode"
            (init, e) <- (braces odeAttribs)
            return (M.ODE init e)
        )
    <|> (do reserved "sde"
            (init, e, wein) <- (braces sdeAttribs)
            return (M.SDE init e wein)
        )
-}
    <|> try (identifier >>=If a CHI paper proposes that a system makes a task easier to accomplish, the is an expectation for a corresponding user study contrasting the task with and without the approach. In contrast, in PL, casting something as tractable viz. analysis or lowering false positive count is sufficient. One of the most informative parts of a paper should be the evaluation: what worked, what didn't, and how did it fit into the bigger picture. Not including an analysis of a person actually using the language for its intended task is suspicious.

People who live in glass houses...
            (\id -> (parens . commaSep $ expr) >>=
            (\exprs -> return (M.FuncCall id exprs))))
    <|> (identifier >>= (\id -> return (M.ValueRef id)))
    <?> "simple expression"

caseAttribs = do
        cases <- endBy caseExpr comma
        reserved "default"
        colon
        def <- expr
        return (cases, def)
  where
    caseExpr = do
        logicalExp <- logicalExpr
        colon
        caseExp <- expr
        return (logicalExp, caseExp)

-- | boolean expression parser - can't use parsec one as boolean expressions aren't recursive - clean up
boolExpr' :: Parser M.BoolExpr
boolExpr' =
    do  a <- expr
        relOp <- relParse
        b <- expr
        return (M.RelExpr a relOp b)
  where
    relParse =
        (do { reservedOp ">"; return A.GT})
        <|> (do { reservedOp ">="; return A.GE})
        <|> (do { reservedOp "<"; return A.LT})
        <|> (do { reservedOp "<="; return A.LE})
        <|> (do { reservedOp "=="; return A.EQ})

logicalExpr  :: Parser M.BoolExpr
logicalExpr  =  buildExpressionParser logicalTable logicalTerm <?> "logical expression"

logicalTable :: OperatorTable Char () M.BoolExpr
logicalTable =
    [[logical "and" A.And AssocLeft], [logical "or" A.Or AssocLeft]
    ]
  where
    logical name binop assoc = Infix (do { reserved name; return (\a b -> M.LogExpr a binop b) } <?> "operator") assoc

-- | value on either side of an operator
logicalTerm :: Parser M.BoolExpr
logicalTerm =
    parens logicalExpr
    <|> boolExpr'


-- | a parameterised single attribute parser for a given attriibute identifier
-- attrib :: String -> Parser String
attrib res p =
    do  reserved res
        colon
        r <- p
        -- TODO - fix the comma separated list of attribute, commaSep?
        skipMany comma
        return r

-- | all the attributes currently used in ode exps
odeAttribs :: Parser (Double, M.Expr)
odeAttribs =
    permute (tuple  <$$> (attrib "init" float)
                    <||> (attrib "delta" term)
                    )
  where
    tuple init e = (init, e)

-- | all the attributes currently used in ode exps
sdeAttribs :: Parser (Double, M.Expr, M.Expr)
sdeAttribs =
    permute (tuple  <$$> (attrib "init" float)
                    <||> (attrib "delta" term)
                    <||> (attrib "weiner" term)
                    )
  where
    tuple init e wein = (init, e, wein)


-- | Parser for a single value definition statement
valueDef :: Parser M.CompStmt
valueDef =
    do  id <- identifier
        reservedOp "="
        val <- expr
        return (M.ValueDef id val)

compCallDef :: Parser M.CompStmt
compCallDef =
    do  outputIds <- parens . commaSep $ identifier
        reservedOp "="
        name <- identifier
        inputExprs <- parens . commaSep $ expr
        return (M.CompCallDef outputIds name inputExprs)

odeDef :: Parser M.CompStmt
odeDef =
    do  id <- identifier
        reservedOp "="
        reserved "ode"
        (init, e) <- (braces odeAttribs)
        return (M.ValueDef id (M.ODE id init e))

sdeDef :: Parser M.CompStmt
sdeDef =
    do  id <- identifier
        reservedOp "="
        reserved "sde"
        (init, e, wein) <- (braces sdeAttribs)
        return (M.ValueDef id (M.SDE id init e wein))

componentDef =
    do  try (compCallDef)
        <|> try (odeDef)
        <|> try (sdeDef)
        <|> valueDef


-- | Parser for the set of statements allowed within a component, including the return values
componentBody :: Parser ([M.CompStmt], [M.Expr])
componentBody =
    do
        body <- endBy componentDef  semi
        -- let cBody = map (\x -> M.CompValueDef x) body
        -- parse the return value, no semi there, so always found as last statement
        reserved "return"
        outputs <- parens (commaSep1 expr)
        return (body,outputs)

-- | Parser for a single component
component :: Parser M.Component
component =
    do  reserved "component"
        id <- identifier
        inputs <- parens (commaSep1 identifier)
        (body, outputs) <- braces (componentBody)
        return (M.Component id inputs outputs body)

-- | all the attributes currently used in first order values
simulateAttribs :: Parser A.Simulate
simulateAttribs =
    permute (tuple  <$$> (attrib "component" identifier)
                    <||> (attrib "param" identifier)
                    <||> (attrib "from" float)
                    <||> (attrib "to" float)
                    <||> (attrib "step" float)
                    <||> (attrib "sample" natural)
                    <||> (attrib "output" identifier)
                    )
  where
    tuple comp par from to step samp out = A.Simulate {A.sComponent = comp, A.sParam = par, A.sFrom = from,
        A.sTo = to, A.sStep = step, A.sSample = samp, A.sFilename = out++".bin"}

-- | Parser for a single value definition statement
simulate :: Parser A.Simulate
simulate =
    do  reserved "simulate"
        -- check for (optional?) braces
        braces simulateAttribs
        --return (M.SimulateStatement sim)

-- | parse a list of expressions ended with semi-colons within the file
odeMain :: Parser M.Model
odeMain =
    do  whiteSpace
        sims <- many1 simulate
        comps <- many1 component
        -- build a table of the components indexed by name
        let compsMap = foldl (\map c -> Map.insert (M.cName c) c map) Map.empty comps
        eof
        return (M.Model sims compsMap)

-- | parses the filename and returns the result if sucessful
-- | maybe move into main
odeParse :: FilePath -> String -> MExcept M.Model
odeParse fileName fileData =
    -- do  parseRes <- parseFromFile odeMain fileName
    case parseRes of
        Left err    -> Left ("parse error at " ++ show err)
        Right res   -> Right res
  where
    parseRes = parse odeMain fileName fileData
