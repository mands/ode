-----------------------------------------------------------------------------
--
-- Module      :  Ode.Desugarer
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- |Desugarer - takes an Ode AST and desguars and converts into the Core langauge AST
-- Can issue errors due to user defined model
--
-----------------------------------------------------------------------------

module Ode.Desugarer (
desugar
) where

import qualified Data.Map as Map
import Utilities
import qualified Ode.AST as O
import qualified Core.AST as C

-- We need a supply of unique Ids
tmpName = "tmp"

-- |desugar function takes an ODE model representaiton and converts it into a lower-level Core AST
-- we only concern ourselves with single module models for now
desugar :: O.Model -> MExcept (C.Model C.Id)
desugar (O.Model files modules) = return a
  where
    -- fold over the list of components within the module creating the model
    a = foldl desugarModElems Map.empty testModElems

    -- filter to get complete modules and return only the elems list
    testModElems =  head . map (\(O.ModuleAbs _ _ elems) -> elems) . filter
                        (\m -> case m of
                            (O.ModuleAbs _ Nothing _) -> True
                            otherwise -> False)
                        $ modules


-- |desugar a top-level value constant(s)
desugarModElems :: (C.Model C.Id) -> O.ModuleElem -> (C.Model C.Id)
desugarModElems map (O.ModuleElemValue v) = map'
  where
    -- need to create a list of lets from the list of ids
    res = C.TopLet tmpName (desugarExpr value)

    -- then call standard expression desugarer



    ids = O.vName v
    value = O.vValue v
    map' = map

desugarExpr :: O.Expr -> C.Expr C.Id
desugarExpr = undefined
