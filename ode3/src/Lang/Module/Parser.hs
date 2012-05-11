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
-- | The main parser frontend, parsers modules of any type that must evaluate to a Core module
-- | Respobible for performing module importing, setting up the module environment and so on
--
-----------------------------------------------------------------------------

module Lang.Module.Parser (
consoleParse, fileParse
) where

import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String
import Debug.Trace (trace)

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Bimap as Bimap
import qualified Utils.OrdMap as OrdMap

import Utils.Utils
import Lang.Common.Parser
import Lang.Module.AST

import qualified Lang.Ode.AST as O
import qualified Lang.Ode.Parser as OP
import qualified Lang.Core.AST as E
--import qualified Lang.Module.ModuleDriver as MD

import Lang.Ode.Desugarer (desugarMod)


-- | lexeme parser for module identifier, return a list of module URI elements
modIdentifier :: Parser ModURIElems
modIdentifier = lexeme $ upperIdentifier `sepBy` (char '.')

singModId :: Parser ModURI
singModId = lexeme upperIdentifier


-- | consoleParse parses a string given on the console command line, restricuted to moduleCmds only
-- example "import X.Y.Z"
consoleParse :: String -> MExceptIO (OdeTopElem E.DesId)
consoleParse cmdStr =   case (parse parser "<console>" cmdStr) of
                            Left err -> throwError ("Input error at " ++ show err)
                            Right res -> return res
  where
    -- parser for a single command string
    parser :: Parser (OdeTopElem E.DesId)
    parser = whiteSpace *> moduleCmd <* eof

-- | fileParse takes an input file and a current snapshot of the module env, and parse within this context
-- sucessfully parsed modules are then converted into (Module E.Id) and added to the env
-- have to explictily case to convert the error type in the Either
fileParse :: FilePath -> String -> ModURI -> MExcept ([OdeTopElem E.DesId])
fileParse fileName fileData canonRoot =  case parse parser fileName fileData of
                                                    Left err -> throwError ("Parse error at " ++ show err)
                                                    Right res -> return res
  where
    -- | parser for an Ode file, containing both module commands and definitions
    parser :: Parser [OdeTopElem E.DesId]
    parser = whiteSpace *> (many1 $ moduleCmd <|> moduleDef) <* eof


-- | modules commands, used to import and setup alias - used from console and files, both at top-level and within module?
moduleCmd :: Parser (OdeTopElem E.DesId)
moduleCmd = try (ModImport <$> (reserved "import" *> modPathImportAll) <*> (pure Nothing))
                <|> importWrap <$> (reserved "import" *> modPathImport) <*> optionMaybe (reserved "as" *> singModId)
                <|> TopMod <$> (reserved "module" *> singModId) <*> (reservedOp "=" *> moduleAppParams)
                -- test, this should be removed
                <|> ModAlias <$> (reserved "module" *> singModId) <*> (reservedOp "=" *> modIdentifier)
                <?> "valid module command"
  where
    importWrap :: ModURIElems -> (Maybe ModURI) -> (OdeTopElem E.DesId)
    importWrap modURI mAlias = ModImport (List.init modURI) (Just [(List.last modURI, mAlias)])

    -- | lexeme parser for a module string in dot notation
    modPathImport :: Parser ModURIElems
    modPathImport = lexeme $ upperIdentifier `sepBy1` (char '.')

    modPathImportAll :: Parser ModURIElems
    modPathImportAll = lexeme $ upperIdentifier `sepEndBy` (char '.') <* (char '*')

    -- | parse a chain/tree of module applications
    moduleAppParams :: Parser (Module E.DesId)
    moduleAppParams = procParams <$> singModId <*> optionMaybe (paramList moduleAppParams)
      where
        -- need to desugar into nested set of appMods and varMods
        procParams modId Nothing = VarMod modId
        procParams funcId (Just args) = AppMod funcId args

-- | module definitions, either an entire definition/abstraction or an application - only used from files
moduleDef :: Parser (OdeTopElem E.DesId)
moduleDef = TopMod <$> (reserved "module" *> singModId) <*> modParse
  where
    modParse =  FunctorMod <$> (funcArgs <$> paramList singModId) <*> pure OrdMap.empty <*> modData
                <|> LitMod <$> pure OrdMap.empty <*> modData
                <?> "module definition"
    modData = ModuleData Map.empty Map.empty Bimap.empty Nothing <$> modBody
    funcArgs args = OrdMap.fromList $ map (\arg -> (arg, Map.empty)) args

    -- | parses a Ode list of statements within module body, and automatically desugars into Core-lang
    modBody :: Parser ExprList
    modBody = do
        modElems <- braces (many1 OP.moduleBody)
        either (\_ -> return []) (\exprList -> return exprList) (desugarMod modElems)
