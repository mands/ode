-----------------------------------------------------------------------------
--
-- Module      :  UI.ShellState
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

module UI.ShellState (
ShState(..), mkDefShState,
RepoSet
) where

import Control.Monad
import Control.Monad.Trans(liftIO)
import Control.Applicative
import qualified Utils.OrdSet as OrdSet
import qualified Lang.Module.AST as MA
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.FilePath as FP
import qualified System.Directory as Dir


-- | Main system state used by the shell
data ShState = ShState  { stDebug :: Bool               -- do we enable debug mode
                        , stSimStart :: Float           -- simulation params
                        , stSimEnd :: Float
                        , stSimTimestep :: Float        -- simulation timestep
                        , stOutPeriod     :: Integer    -- period with which to save simulation state to outfile, wrt timestep
                        , stOutFilename :: FilePath     -- output filename to save data to
                        , stRepos :: RepoSet       -- list of enabled module repositories
                        , stModuleEnv :: MA.ModuleEnv   -- map of loaded modules
                        , stParsedFiles :: Set.Set FilePath  -- a set of fully parsed files
                        -- what else??
                        } deriving Show

-- | Sensible default values for initial system state
defShState = ShState   { stDebug = False
                        , stSimStart = 0
                        , stSimEnd = 60
                        , stSimTimestep = 0.001         -- 1ms
                        , stOutPeriod = 500             -- 0.5s
                        , stOutFilename = "output.bin"  -- default output file
                        , stRepos = OrdSet.empty           -- do we add the defaults here?
                        , stModuleEnv = Map.empty
                        , stParsedFiles = Set.empty
                        }

mkDefShState :: IO ShState
mkDefShState = do
    repos <- defRepos
    return $ defShState { stRepos = repos }


-- | Holds the ordered set of enabled repositories
type RepoSet = OrdSet.OrdSet FilePath

-- need to take out of IOdef
defRepos :: IO RepoSet
defRepos = repos
  where
    repos = liftM OrdSet.fromList $ sequence [curDir, userDir]
    curDir = Dir.getCurrentDirectory
    userDir = FP.combine <$> Dir.getAppUserDataDirectory "ode" <*> pure "repos"
    sysDir = undefined
