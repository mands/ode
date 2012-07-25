-- | Special interface to allow evalImport to be called from ModDefDriver, breaking the import cycle between
-- ModCmdDriver and ModDefDriver

module Lang.Module.ModCmdDriver (
evalImport
) where

import Utils.Utils
import qualified SysState as St
import Lang.Common.AST
import Lang.Module.AST

evalImport :: ImportMap -> ModImport -> St.SysExceptIO ImportMap
