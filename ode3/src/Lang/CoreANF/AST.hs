-----------------------------------------------------------------------------
--
-- Module      :  CoreANF.AST
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |A represetnation of the Core AST in Administrative Normal Form (ANF)
-- sohuld this also be in SSA form/FIR/??
-- should contain menstructions, single statments/ops only?
-- check Appel book
--
-----------------------------------------------------------------------------

module CoreANF.AST (
Model
) where


type Model = [Int]
