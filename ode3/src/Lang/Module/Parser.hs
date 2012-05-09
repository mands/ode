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
modParse, parseModCmd
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
import qualified Data.Set as Set
import qualified Data.Bimap as Bimap
import qualified Utils.OrdMap as OrdMap

import Utils.Utils
import Lang.Common.Parser
import Lang.Module.AST

import qualified Lang.Ode.AST as O
import qualified Lang.Ode.Parser as OP
import qualified Lang.Core.AST as E
import qualified Lang.Module.ModuleDriver as MD
import Lang.Ode.Desugarer (desugarMod)


-- TODO - put into IO
-- example "import X.Y.Z"
parseModCmd :: String -> MExceptIO ModCmd
parseModCmd cmdStr =  case parseRes of
                            Left err -> throwError ("Input error at " ++ show err)
                            Right res -> return res
  where
    parseRes = parse (cmdModuleOpen <* eof) "<console>" cmdStr


-- shell cmd parsers - need to unify to main parser?
-- | parse the open directive
cmdModuleOpen :: Parser ModCmd
cmdModuleOpen = ModImport <$> (reserved "import" *> modPathIdentifier) <*> optionMaybe (reserved "as" *> upperIdentifier)


-- | modParse takes an input file and a current snapshot of the module env, and parse within this context
-- sucessfully parsed modules are then converted into (Module E.Id) and added to the env
-- have to explictily case to convert the error type in the Either
modParse :: FilePath -> String -> ModURI -> ModuleEnv ->  MExcept ModuleEnv
modParse fileName fileData canonRoot modEnv = case parseRes of
                                        Left err -> throwError ("Parse error at " ++ show err)
                                        Right res -> res
  where
    parseRes = parse (modFileTop canonRoot modEnv) fileName fileData

-- | top level parser for a file
modFileTop :: ModURI -> ModuleEnv -> Parser (MExcept ModuleEnv)
modFileTop canonRoot modEnv = do
    imports <- (whiteSpace *> many moduleOpen)
    -- TODO, should lookup the imports here and update the env

    -- update the env and now parse the modules using the new env
    mods <- many1 (moduleDef modEnv) <* eof

    -- HACK - filter and leave only LitMods for now
    let mods' = filter filterLit mods

    -- instantiate each module and add to the moduleEnv
    let modEnv' = DF.foldlM (MD.moduleDriver canonRoot) modEnv mods'
    return $ trace ("(MP) " ++ show imports) (trace ("(MP) " ++ show mods') modEnv')

  where
    filterLit m = case m of
        (TopMod _ (LitMod _ _)) -> True
        _ -> False

-- | parse the open directive
moduleOpen :: Parser ModURIElems
moduleOpen = reserved "import" *> modPathIdentifier

-- | parse a module, either an entire definition/abstraction or an application
moduleDef :: ModuleEnv -> Parser (TopMod E.DesId)
moduleDef modEnv = TopMod <$> (reserved "module" *> modIdentifier) <*> modParse
  where
    modParse =
        (reservedOp "=" *> moduleAppParams)
        <|> FunctorMod <$> (funcArgs <$> paramList modIdentifier) <*> pure OrdMap.empty <*> modData
        <|> LitMod <$> pure OrdMap.empty <*> modData
        <?> "module definition"
    modData = ModuleData Map.empty Map.empty Bimap.empty Nothing <$> modBody
    funcArgs args = OrdMap.fromList $ map (\arg -> (arg, Map.empty)) args

-- | parse a chain/tree of module applications
moduleAppParams :: Parser (Module E.DesId)
moduleAppParams = procParams <$> modIdentifier <*> optionMaybe (paramList moduleAppParams)
  where
    -- need to desugar into nested set of appMods and varMods
    procParams modId Nothing = VarMod modId
    procParams funcId (Just args) = AppMod funcId args


-- | parses a Ode module body and desugars into a Core AST
modBody :: Parser (ExprList)
modBody = do
    modElems <- braces (many1 OP.moduleBody)
    either (\_ -> return []) (\exprList -> return exprList) (desugarMod modElems)


