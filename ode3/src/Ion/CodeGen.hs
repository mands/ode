-----------------------------------------------------------------------------
--
-- Module      :  Ion.CodeGen
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Ion.CodeGen (
ionCodeGen
) where


import Prelude hiding (div)
import System.Log.Logger
import System.Log.Handler(close)
import System.Log.Handler.Simple

import qualified Data.Set as Set
import AST.CoreFlat(SimType(..))

import Ion.AST
import Ion.Process

import Utils.CommonImports hiding ((<$>))
import Text.PrettyPrint.Leijen

ionCodeGen :: [IonChannel] -> MExcept String
ionCodeGen ionChans = do
    -- create the docs
    let doc = (vcat $ map genChannel ionChans) <$> text "EOF"
    return $ show doc


genChannel :: IonChannel -> Doc
genChannel ionChan@IonChannel{..} = codeBlock modHeader mainComponent
  where
    -- mod header
    modHeader = text "module" <+> text (capitalise name)

    -- main component header
    mainComponent = codeBlock compHeader $ initVals <$> stateVals <$> currentCalc
    compHeader = text "component" <+> text "getCurrent" <> parens voltage

    -- initial vals
    initVals = vsep . map genInitVal $ Set.toList states
      where
        genInitVal state = text "init" <+> text state <+> text "=" <+> initVal
          where
            initVal | initialState == state  = double 1.0
                    | otherwise             = double 0.0

    -- generate converted data depedning on simtype
    stateVals = case simType of
        SimODE -> vsep . map genOdeExpr $ zip (Set.toList states) detElems
          where
            detElems = getDetElems ionChan
            genOdeExpr (initVal, deltaExpr) = text "ode" <+> genAttribs [("init", text initVal)] <+> equals <+> genExpr deltaExpr

        SimSDE -> -- trace' [MkSB detElems, MkSB stocElems] "Eqns" $
            vsep . map genSdeExpr $ zip3 (Set.toList states) detElems stocElems
          where
            detElems = getDetElems ionChan
            stocElems = getStocElems ionChan
            genSdeExpr (initVal, deltaExpr, weinerExpr) = text "sde" <+> genAttribs [("init", text initVal), ("weiner", genExpr weinerExpr)] <+> equals <+> genExpr deltaExpr

        SimRRE -> vsep . concat . map genRreExpr $ transitions
          where
            genRreExpr Transition{..} = [genRreExpr' stateA stateB fRate, genRreExpr' stateB stateA rRate]
            genRreExpr' x y eRate = text "rre" <+> genAttribs [("rate", genExpr eRate)] <+> equals <+> text x <+> text "->" <+> text y

    currentCalc = defI <$> retI
      where
        defI = text "val" <+> text "current" <+> equals <+> exprI
        retI = text "return" <+> text "current"
        -- eqn taken from rudy/silva paper
        exprI = double chanConductance <> mul <> double density <> mul <> parenSep (map text openStates) <> mul <> parens (voltage <> minus <> double eqPot)
        parenSep = encloseSep lparen rparen plus

-- independent gen combinators

-- | convert an ion expression AST into source-code - not tail-call form
-- TODO - fix paren generation
genExpr :: IonExpr -> Doc
genExpr (Var x) = text x
genExpr (Num n) = double n
-- math ops
genExpr (Add e1 e2) = (parens $ genExpr e1) <> plus <> (parens $ genExpr e2)
genExpr (Mul e1 e2) = (parens $ genExpr e1) <> mul <> (parens $ genExpr e2)
genExpr (Sub e1 e2) = (parens $ genExpr e1) <> minus <> (parens $ genExpr e2)
genExpr (Div e1 e2) = (parens $ genExpr e1) <> div <> (parens $ genExpr e2)
-- bin ops
genExpr (Neg e1) = char '-' <> (parens $ genExpr e1)
genExpr (Sqrt e1) = text "sqrt" <> (parens $ genExpr e1)


genAttribs :: [(String, Doc)] -> Doc
genAttribs as = encloseSep lbrace rbrace comma $ map attrib as
  where
    attrib :: (String, Doc) -> Doc
    attrib (n, val) = text n <+> colon <+> val

-- | Nest a bit of code using our preferred style
codeNest :: Doc -> Doc
codeNest x = braces (nest 4 $ linebreak <> x <> linebreak)

codeBlock :: Doc -> Doc -> Doc
codeBlock header content = header <+> codeNest content

-- basic combinators
-- TODO - change mul and plus to implement spacing
mul = char '*'
plus = char '+'
minus = char '-'
div = char '/'
voltage = char 'V'
