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
import Control.Monad
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String
import Debug.Trace (trace)

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Utils.OrdMap as OrdMap

import Utils.Utils
import Common.Parser
import qualified Ode.AST as O
import qualified Ode.Parser as OP
import qualified Core.ModuleAST as M
import qualified Core.ExprAST as E
import qualified Core.ModuleDriver as MD
import Ode.Desugarer (desugarMod)

-- | modParse takes an input file and a current snapshot of the module env, and parse within this context
-- sucessfuylly parsed modules are then converted into (Module E.Id) and added to the env
modParse :: FilePath -> String -> M.ModuleEnv ->  MExcept M.ModuleEnv
modParse fileName fileData modEnv =
    case parseRes of
        Left err -> Left ("Parse error at " ++ show err)
        Right res -> res
  where
    parseRes = parse (modFileTop modEnv) fileName fileData

-- | top level parser for a file
modFileTop :: M.ModuleEnv -> Parser (MExcept M.ModuleEnv)
modFileTop modEnv = do
    imports <- (whiteSpace *> many moduleOpen)
    -- TODO, should lookup the imports here and update the env

    -- update the env and now parse the modules using the new env
    mods <- many1 (moduleDef modEnv) <* eof

    -- HACK - filter and leave only LitMods for now
    let mods' = filter filterLit mods

    -- add the new mods to the moduleEnv
    -- TODO - output, don't swallow the error msg here
    let modEnv' = DF.foldlM MD.newModuleDriver modEnv mods'
    return $ trace ("(MP) " ++ show imports) (trace ("(MP) " ++ show mods') modEnv')

  where
    filterLit m = case m of
        (M.TopMod _ (M.LitMod _ _)) -> True
        _ -> False

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


-- TODO - errors during desugaring are swallowed
-- | parses a Ode module body and desugars into a Core AST
modBody :: Parser (M.ExprMap E.SrcId)
modBody = do
    modElems <- braces (many1 OP.moduleBody)
    case (desugarMod modElems) of
        Left err -> return OrdMap.empty
        Right exprMap -> return exprMap

-- Util functions - need to refactor and place elsewhere
-- | Function that takes an ODE Module and fully converts it into a Core Module
-- (i.e. desugar, reorder, rename, typecheck) with respect to the current ModuleEnv
--odeCoreConvert :: M.ModuleEnv -> (M.TopMod E.SrcId)  -> MExcept (M.ModuleEnv)
--odeCoreConvert modEnv mod = MD.newModuleDriver modEnv mod

















