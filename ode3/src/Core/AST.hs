-----------------------------------------------------------------------------
--
-- Module      :  Core.AST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |A lower-level desuagred AST that represents the mian language features
-- is parameterised by the types of values
-- a reference interpreter exists that may execute on the type-checked Core AST
-- bascially the lambda-calculus style IR
-- should be a functor with fmap defined
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies  #-}
{-# LANGUAGE GADTs, EmptyDataDecls, KindSignatures #-}

-- just reexports the imports
module Core.AST (
SrcId, Id, ModId, Bind(..), Type(..), travTypes, getTopBinding,
Top(..), Expr(..), Op(..), Literal(..),
TopMod(..), Module(..), ModuleData(..), ModuleEnv, ExprMap, SigMap, TypeMap, IdBimap,
) where

import Core.AST.Expr
import Core.AST.Module
