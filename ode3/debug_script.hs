:m +*Lang.Core.Units.Conversion
:m +*Lang.Core.Units

-- unitenv
let (Right unitEnv) = addUnitsToEnv defUnits [UnitDef "m" (getBaseDim 'L'), UnitDef "mm" (getBaseDim 'L'), UnitDef "km" (getBaseDim 'L'), UnitDef "ft" (getBaseDim 'L')]

-- add conversion funcs here
-- splitUnit [("sad", 1)] unitEnv

-- splitUnit [("m", 1), ("mm", -1), ("m", 2), ("mm", 3)] unitEnv

-- sample units
-- simplifyUnits [("m", 2)] [("ft", 2)] unitEnv
-- simplifyUnits [("m", 1), ("mm", 1)] [("ft", 1), ("km", 1)] unitEnv

-- simplifyUnits [("m", 3), ("mm", -2)] [("m", 1)] unitEnv
-- simplifyUnits [("m", 3), ("mm", -2)] [("m", 1), ("m", -2)] unitEnv
-- simplifyUnits [("m", 3), ("mm", -2)] [("m", 1), ("m", -2)] unitEnv

-- simplifyUnits [("m", 2), ("mm", -1)] [("m", 1)] unitEnv
-- simplifyUnits [("ft", 1), ("mm", 1)] [("m", 2)] unitEnv
-- simplifyUnits [("mm", 1)] [("m", 2), ("ft", -1)] unitEnv


