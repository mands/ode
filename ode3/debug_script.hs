:m +*Lang.Core.Units

import qualified Data.Map as Map
import qualified Utils.Graph as UG

let fromRight (Right r) = r

{- 
let units = builtinUnits

let uEnv = fromRight $ addUnitsToEnv Map.empty units

let cEnv = fromRight $ addConvsToGraph Map.empty builtinConvs uEnv
-}

let gm = UG.mkGraphMap


