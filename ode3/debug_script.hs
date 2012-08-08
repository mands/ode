:m +*Lang.Core.Units.Conversion
:m +*Lang.Core.Units

-- unitenv
let (Right unitEnv) = addUnitsToEnv defUnits [UnitDef "m" DimL , UnitDef "mm" DimL, UnitDef "km" DimL, UnitDef "ft" DimL]

-- add conversion funcs here
let (Right convEnv) = addConvsToGraph defConvs [ ConvDef "mm" "m" (CExpr CDiv CFromId (CNum 1000)), ConvDef "m" "mm" (CExpr CMul CFromId (CNum 1000)) , ConvDef "m" "km" (CExpr CDiv CFromId (CNum 1000)) , ConvDef "km" "m" (CExpr CMul CFromId (CNum 1000)), ConvDef "ft" "m" (CExpr CDiv CFromId (CNum 3.28084)), ConvDef "m" "ft" (CExpr CMul CFromId (CNum 3.28084))] unitEnv



-- splitUnit [("sad", 1)] unitEnv
-- splitUnit [("m", 1), ("mm", -1), ("m", 2), ("mm", 3)] unitEnv

-- sample units
-- cancel test - works
-- simplifyUnits (mkUnit [("m", 1)]) (mkUnit [("m", 1)]) unitEnv convEnv
-- simple conv test
-- simplifyUnits (mkUnit [("m", 1)]) (mkUnit [("mm", 1)]) unitEnv convEnv

-- simplifyUnits (mkUnit [("m", 2)]) (mkUnit [("ft", 2)]) unitEnv convEnv
-- simplifyUnits (mkUnit [("m", 1), ("mm", 1)]) (mkUnit [("ft", 1), ("km", 1)]) unitEnv convEnv

-- simplifyUnits (mkUnit [("m", 3), ("mm", -2)]) (mkUnit [("m", 1)]) unitEnv convEnv
-- simplifyUnits (mkUnit [("m", 3), ("mm", -2)]) (mkUnit [("m", 1), ("m", -2)]) unitEnv convEnv
-- simplifyUnits (mkUnit [("m", 3), ("mm", -2)]) (mkUnit [("m", 1), ("m", -2)]) unitEnv convEnv

-- simplifyUnits (mkUnit [("m", 2), ("mm", -1)]) (mkUnit [("m", 1)]) unitEnv convEnv
-- simplifyUnits (mkUnit [("ft", 1), ("mm", 1)]) (mkUnit [("m", 2)]) unitEnv convEnv
-- simplifyUnits (mkUnit [("mm", 1)]) (mkUnit [("m", 2), ("ft", -1)]) unitEnv convEnv


