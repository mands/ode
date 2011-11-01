-----------------------------------------------------------------------------
--
-- Module      :  Core.ModuleParser
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | The main parser forntend, parsers modules of any type that must evaluate to a Core module
-- | Respobible for performing module importing, setting up the module environment and so on
--
-----------------------------------------------------------------------------

module Core.ModuleParser (
modParse
) where

import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import Control.Applicative
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language( javaStyle )
import Text.Parsec.Perm
import Debug.Trace (trace)

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Utils.OrdMap as OrdMap

import Utils.Utils
import qualified Ode.AST as O
import qualified Ode.Parser as OP
import qualified Core.ModuleAST as M
import qualified Core.ExprAST as E
import qualified Core.ModuleDriver as MD
import Ode.Desugarer (desugarMod)

-- | hijack the javaStyle default definition, gives us a bunch of ready-made parsers/behaviours
coreLangDef = javaStyle
    {
        -- add more later
        T.reservedNames =   ["module", "import", "as"],
        -- unary ops and relational ops?
        -- do formatting operators count? e.g. :, {, }, ,, ..,  etc.
        -- NO - they are symbols to aid parsiing and have no meaning in the language itself...
        T.reservedOpNames = ["="],
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

-- | lexeme parser for module identifier
modIdentifier :: Parser String
modIdentifier = lexeme upperIdentifier

-- | lexeme parser for a module string in dot notation
modPathIdentifier :: Parser [String]
modPathIdentifier = lexeme $ upperIdentifier `sepBy1` (char '.')

-- | parses a upper case identifier
upperIdentifier :: Parser String
upperIdentifier = (:) <$> upper <*> many alphaNum <?> "module identifier"

-- | comma sepated parameter list of any parser, e.g. (a,b,c)
paramList = parens . commaSep

-- | modParse takes an input file and a current snapshot of the module env, and parse within this context
-- sucessfuylly parsed modules are then converted into (Module E.Id) and added to the env
modParse :: FilePath -> String -> M.ModuleEnv ->  MExcept M.ModuleEnv
modParse fileName fileData modEnv = case parseRes of
    Left err -> Left ("Parse error at " ++ show err)
    Right res -> Right res
  where
    parseRes = parse (modFileTop modEnv) fileName fileData

-- | top level parser for a file
modFileTop :: M.ModuleEnv -> Parser M.ModuleEnv
modFileTop modEnv = do
    imports <- (whiteSpace *> many moduleOpen)
    -- TODO, should lookup the imports here and update the env

    -- update the env and now parse the modules using the new env
    mods <- many1 (moduleDef modEnv) <* eof

    -- add the new mods to the moduleEnv
    let modEnv' = either (\_ -> modEnv) id $ DF.foldlM odeCoreConvert modEnv mods
    return $ trace (show imports) (trace (show mods) modEnv')


-- | parse the open directive
moduleOpen :: Parser M.ModImport
moduleOpen = reserved "import" *> modPathIdentifier

-- | parse a module, either an entire definition/abstraction or an application
moduleDef :: M.ModuleEnv -> Parser (M.TopMod E.SrcId)
moduleDef modEnv = M.TopMod <$> (reserved "module" *> modIdentifier) <*> modParse
  where
    modParse =
        (reservedOp "=" *> moduleAppParams)
        <|> M.FunctorMod <$> (funcArgs <$> paramList modIdentifier) <*> modBody <*> pure modData
        <|> M.LitMod <$> modBody <*> pure modData
        <?> "module definition"
    modData = M.ModuleData Map.empty Map.empty Bimap.empty Nothing
    funcArgs args = OrdMap.fromList $ map (\arg -> (arg, Map.empty)) args

-- | parse a chain/tree of module applications
moduleAppParams :: Parser (M.Module E.SrcId)
moduleAppParams = procParams <$> modIdentifier <*> optionMaybe (paramList moduleAppParams)
  where
    -- need to desugar into nested set of appMods and varMods
    procParams modId Nothing = M.VarMod modId
    procParams funcId (Just args) = M.AppMod funcId args

modBody :: Parser (M.ExprMap E.SrcId)
modBody = do
    modElems <- braces (many1 OP.moduleBody)
    case (desugarMod modElems) of
        Left err -> return OrdMap.empty
        Right exprMap -> return exprMap

-- Util functions - need to refactor and place elsewhere
-- | Function that takes an ODE Module and fully converts it into a Core Module
-- (i.e. desugar, reorder, rename, typecheck) with respect to the current ModuleEnv
odeCoreConvert :: M.ModuleEnv -> (M.TopMod E.SrcId)  -> MExcept (M.ModuleEnv)
odeCoreConvert modEnv mod = MD.newModuleDriver modEnv mod




















