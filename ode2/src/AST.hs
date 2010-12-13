-----------------------------------------------------------------------------
--
-- Module      :  AST
-- Copyright   :  Copyright (C) 2010 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Common values and types to all AST representations
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}

module AST (
    NumOp(..), RelOp(..), LogOp(..), Id, NumTy, ConvertAST(..), Simulate(..), uFuncs, bFuncs
) where

import Control.Monad.Error

-- common types to all ASTs
-- basic types - make newtype??
type Id = String
type NumTy = Double
--type BooleanType = Bool

data Simulate = Simulate { sComponent :: Id, sParam :: Id, sFrom :: NumTy,
                           sTo :: NumTy, sStep :: NumTy, sSample :: Integer, sFilename :: String }
                           deriving Show

data NumOp  = Add | Sub | Mul | Div deriving Show

-- piecewise definition, complicates everything considerably...
data RelOp = LT | LE | GT | GE | EQ deriving Show
data LogOp  = And | Or | Not deriving Show

-- | standard conversion operations for converting ASTs
class ConvertAST a b where
    convert :: a -> b

-- basic runtime env, a set of functions
-- need to create a set of primitive functions
-- e.g. sin/cos, etc., others?
-- maybe uses a run time env or subsume into exp env,
-- could inline into the typecheck stage and embed directly into ast, rather than lookup
uFuncs :: [(Id, NumTy -> NumTy)]
uFuncs =
    [("sin", sin)
    ,("cos", cos)
    ,("tan", tan)
    ,("sinh", sinh)
    ,("cosh", cosh)
    ,("tanh", tanh)
    ,("exp", exp)
    ,("log", log)
    ]

bFuncs :: [(Id, NumTy -> NumTy -> NumTy)]
bFuncs =
    [("pow", \x y -> (^) x (round y)) -- bit slow pow function, ah well
    ,("min", min)
    ,("max", max)
    ]

