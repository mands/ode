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


import Control.Applicative
import Control.Monad
import Control.Monad.Error

import qualified Data.Traversable as DT
import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Bimap as Bimap
import qualified Utils.OrdMap as OrdMap

import Text.Parsec hiding (many, optional, (<|>))
import Text.Printf (printf)

import Utils.Utils
import Lang.Common.Parser
import qualified Lang.Ode.Parser as OP
import Lang.Common.AST
import Lang.Module.AST
import qualified Lang.Ode.AST as O
import qualified Lang.Core.AST as E
import Lang.Ode.Desugarer



-- | consoleParse parses a string given on the console command line, restricuted to moduleCmds only
-- example "import X.Y.Z"
consoleParse :: String -> MExceptIO (OdeTopElem E.DesId)
consoleParse cmdStr =   case (runParser parser () "<console>" cmdStr) of
                            Left err -> throwError ("Input error at " ++ show err)
                            Right res -> return res
  where
    -- parser for a single command string
    parser :: Parser (OdeTopElem E.DesId)
    parser = whiteSpace *> topModImport <|> moduleCmd <* eof

-- | fileParse takes an input file and a current snapshot of the module env, and parse within this context
-- sucessfully parsed modules are then converted into (Module E.Id) and added to the env
-- have to explictily case to convert the error type in the Either
fileParse :: FilePath -> String -> ModRoot -> MExcept [OdeTopElem E.DesId]
fileParse fileName fileData modRoot = case runParser parser () fileName fileData of
                                        Left err -> throwError ("Parse error at " ++ show err)
                                        Right res -> return res
  where
    -- | parser for an Ode file, containing both module commands and definitions
    parser :: Parser [OdeTopElem E.DesId]
    parser = whiteSpace *> (many1 $ topModImport <|> moduleCmd <|> moduleDef modRoot) <* eof

topModImport = TopModImport <$> importCmd

-- | modules commands, used to import and setup alias - used from console and files, both at top-level and within module?
moduleCmd :: Parser (OdeTopElem E.DesId)
moduleCmd = modCmdParse
  where
    modCmdParse :: Parser (OdeTopElem E.DesId)
    modCmdParse =   try (TopModDef <$> pure (mkModRoot [""]) <*> (reserved "module" *> singModId) <*> (reservedOp "=" *> moduleAppParams))
                    -- test, this should be removed
                    -- <|> try (ModAlias <$> (reserved "module" *> singModId) <*> (reservedOp "=" *> singModId))
                    <?> "valid module command"

    -- | parse a chain/tree of module applications
    moduleAppParams :: Parser (Module E.DesId)
    moduleAppParams = procParams <$> singModId <*> optionMaybe (paramList moduleAppParams)
      where
        -- need to desugar into nested set of appMods and varMods
        procParams modId Nothing = VarMod modId
        procParams funcId (Just args) = AppMod funcId args

-- | module definitions, either an entire definition/abstraction or an application - only used from files
moduleDef :: ModRoot -> Parser (OdeTopElem E.DesId)
moduleDef modRoot = TopModDef <$> pure modRoot <*> (reserved "module" *> singModId) <*> modParse
  where
    modParse =  FunctorMod <$> (funcArgs <$> paramList singModId) <*> pure OrdMap.empty <*> modData
                <|> LitMod <$> pure OrdMap.empty <*> modData
                <?> "module definition"

    modData = do
        DesugarModData exprList q u importCmds c typeSet <- braces modBody
        return $ mkModData { modImportCmds = importCmds, modExprList = exprList, modQuantities = q, modUnits = u, modConvs = c, modTypes = typeSet }

    funcArgs args = OrdMap.fromList $ map (\arg -> (arg, Map.empty)) args

    -- | parses a Ode list of statements within module body, and automatically desugars into Core-lang
    modBody :: Parser DesugarModData
    modBody = do
        -- parse the module imports here
        -- (imports, modElems) <- braces ((,) <$> many importCmd <*> )
        odeStmts <- many1 OP.odeStmt
        case desugarOde odeStmts of
            Left err -> unexpected $ printf "Unexpected error whilst desugaring Ode into Core, \n%s" (show err)
            Right desugarModData -> return desugarModData
