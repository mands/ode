-----------------------------------------------------------------------------
--
-- Module      :  Utils.CommonImports
-- Copyright   :  Copyright (C) 2010-2012 Mandeep Gill
-- License     :  GPL (Just (Version {versionBranch = [3], versionTags = []}))
--
-- Maintainer  :  mangil@comlab.ox.ac.uk
-- Stability   :  alpha
-- Portability :
--
-- | Just re-export some commonly used modules, must make sure that the names are all distinct
-- thus can't re-export containers stuff
--
-----------------------------------------------------------------------------

module Utils.CommonImports (

    -- whole modules
    module Control.Applicative,
    module Control.Monad.Trans,
    module Control.Monad,
    module Control.Monad.Error,
    module System.Log.Logger,

    -- ode modules
    module Utils.Utils,

    -- indiv functions
    printf, assert
) where

-- higher-level control
import Control.Applicative
import Control.Monad.Trans
import Control.Monad
import Control.Monad.Error
import qualified Control.Monad.State as S
import qualified Control.Conditional as Cond

-- fclabels stuff
import Control.Category
import Data.Label
import Prelude hiding ((.), id)

-- containers
import qualified Data.Foldable as DF
import qualified Data.Map as Map
import qualified Utils.OrdSet as OrdSet
import qualified Data.Set as Set
import qualified Data.List.Split as ListSplit
import Data.Maybe (isJust, fromJust)

-- other
import Text.Printf (printf)
import GHC.Base (assert)
import System.Log.Logger
import qualified System.Directory as Dir
import qualified System.FilePath as FP

-- Ode
import Utils.Utils
