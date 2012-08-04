:m +*Lang.Core.Units

-- splitUnit [("m", 1), ("mm", -1), ("m", 2), ("mm", 3)]

-- sample units
simplifyUnits [("m", 2)] [("ft", 2)]
simplifyUnits [("m", 1), ("mm", 1)] [("ft", 1), ("km", 1)]

simplifyUnits [("m", 3), ("mm", -2)] [("m", 1)]
simplifyUnits [("m", 3), ("mm", -2)] [("m", 1), ("m", -2)]
simplifyUnits [("m", 3), ("mm", -2)] [("m", 1), ("m", -2)]

simplifyUnits [("m", 2), ("mm", -1)] [("m", 1)]
simplifyUnits [("ft", 1), ("mm", 1)] [("m", 2)]
simplifyUnits [("mm", 1)] [("m", 2), ("ft", -1)]

