import cog

class MathLib:
    """Enumeration of possible content types"""
    (Intel, AMD, GNU) = range(3)

## All functions implemented by VecMath
# basic functions of type f->f
# TODO - 'invsqrt', 'invcbrt',
funcsFtoF = { 'sin', 'cos', 'tan'           # trig funs
            , 'asin', 'acos', 'atan'
            , 'sinh', 'cosh', 'tanh'
            , 'asinh', 'acosh', 'atanh'
            , 'exp', 'exp2', 'exp10'        # exp funcs
            , 'log', 'log2', 'log10', 'logb'# log funcs
            , 'sqrt', 'cbrt', 'pow10'       # power funcs
            , 'erf', 'erfc'                 # err funcs
            }
funcsFFtoF =    { 'atan2', 'pow', 'hypot' }

# TODO - not yet implemented
funcsFtoFF =    {'sincos'}

## Helper funcs
def _getLLVMType(vecSize):
    if vecSize == 1:
        return "double"
    else:
        return "<{0} x double>".format(vecSize)

def _getVecMathName(funcName, vecSize):
    if vecSize == 1:
        funcSuffix = "f64"
    else:
        funcSuffix = "v{0}f64".format(vecSize)
    return "vecmath_{0}_{1}".format(funcName, funcSuffix)

def _checkVecSize(vecSize):
    if vecSize in {1, 2, 4}:
        return True
    else:
        raise ValueError('vecSize not a valid number')

def genProto(protoName, vecSize=1, inArgs=1, outArgs=1):
    _checkVecSize(vecSize)
    llvmType = _getLLVMType(vecSize)
    if inArgs==1 and outArgs ==1:
        cog.outl("declare {0} @{1}({0} %ins) nounwind readnone".format(llvmType, protoName))
    elif inArgs==2 and outArgs ==1:
        cog.outl("declare {0} @{1}({0} %ins, {0} %ins2) nounwind readnone".format(llvmType, protoName))
    else:
        raise ValueError("Invalid in or out-args size")


# a wrapper to call the correct genThunk based on the input params
def genThunk(funcName, callName, inVecSize=1, callVecSize=1, inArgs=1, outArgs=1):
    _checkVecSize(inVecSize)
    _checkVecSize(callVecSize)
    vecMathName = _getVecMathName(funcName, inVecSize)
    if inVecSize == callVecSize:
        if inArgs==1 and outArgs ==1:
            _getThunk_0_1in_1out(vecMathName, callName, inVecSize)
        elif inArgs==2 and outArgs ==1:
            _getThunk_0_2in_1out(vecMathName, callName, inVecSize)
    elif inVecSize == 2 and callVecSize == 1:
        if inArgs==1 and outArgs ==1:
            _getThunk_2to1_1in_1out(vecMathName, callName)
        elif inArgs==2 and outArgs ==1:
            _getThunk_2to1_2in_1out(vecMathName, callName)
#    elif inVecSize == 4 and callVecSize == 1:
#        if inArgs==1 and outArgs ==1:
#            _getThunk_4to1_1in_1out(vecMathName, callName)
#    elif inVecSize == 4 and callVecSize == 2:
#        if inArgs==1 and outArgs ==1:
#            _getThunk_4to2_1in_1out(vecMathName, callName)
    pass

## All our basic "templates" for calling the vectorised math funcs go here
def _getThunk_0_1in_1out(vecMathName, callName, vecSize):
    """calls a packed func directly"""
    llvmType = _getLLVMType(vecSize)
    ## output the func definition
    cog.outl("define {0} @{1}({0} %ins0) nounwind alwaysinline readnone {{".format(llvmType, vecMathName))
    cog.outl("entry:")
    cog.outl("  %rets0 = tail call {0} @{1}({0} %ins0) nounwind readnone".format(llvmType, callName))
    cog.outl("  ret {0} %rets0".format(llvmType))
    cog.outl("}")
    cog.outl("")

## Second set of templates to handle (F, F)->F
def _getThunk_0_2in_1out(vecMathName, callName, vecSize):
    """calls a packed func directly"""
    llvmType = _getLLVMType(vecSize)
    ## output the func definition
    cog.outl("define {0} @{1}({0} %ins0, {0} %ins1) nounwind alwaysinline readnone {{".format(llvmType, vecMathName))
    cog.outl("entry:")
    cog.outl("  %rets0 = tail call {0} @{1}({0} %ins0, {0} %ins1) nounwind readnone".format(llvmType, callName))
    cog.outl("  ret {0} %rets0".format(llvmType))
    cog.outl("}")
    cog.outl("")

def _getThunk_2to1_1in_1out(vecMathName, callName):
    """calls a packed func by unpacking a 2xdouble vector to singles"""
    inType = _getLLVMType(2)
    callType = _getLLVMType(1)
    baseType = _getLLVMType(1)
    ## output the func definition
    cog.outl("define {0} @{1}({0} %ins0) nounwind alwaysinline readnone {{".format(inType, vecMathName))
    cog.outl("entry:")
    # unpack the ins and call
    cog.outl("  %ins0.0 = extractelement {0} %ins0, i32 0".format(inType))
    cog.outl("  %ret0 = tail call {0} @{1}({0} %ins0.0) nounwind readnone".format(callType, callName))
    cog.outl("  %ins0.1 = extractelement {0} %ins0, i32 1".format(inType))
    cog.outl("  %ret1 = tail call {0} @{1}({0} %ins0.1) nounwind readnone".format(callType, callName))
    # repack, and return
    cog.outl("  %rets0.0 = insertelement {0} undef, {1} %ret0, i32 0".format(inType, baseType))
    cog.outl("  %rets0.1 = insertelement {0} %rets0.0, {1} %ret1, i32 1".format(inType, baseType))
    cog.outl("  ret {0} %rets0.1".format(inType))
    cog.outl("}")
    cog.outl("")

def _getThunk_2to1_2in_1out(vecMathName, callName):
    """calls a packed func by unpacking a 2xdouble vector to singles"""
    inType = _getLLVMType(2)
    callType = _getLLVMType(1)
    baseType = _getLLVMType(1)
    ## output the func definition
    cog.outl("define {0} @{1}({0} %ins0, {0} %ins1) nounwind alwaysinline readnone {{".format(inType, vecMathName))
    cog.outl("entry:")
    # unpack the ins and call
    cog.outl("  %ins0.0 = extractelement {0} %ins0, i32 0".format(inType))
    cog.outl("  %ins1.0 = extractelement {0} %ins1, i32 0".format(inType))
    cog.outl("  %ret0 = tail call {0} @{1}({0} %ins0.0, {0} %ins1.0) nounwind readnone".format(callType, callName))
    cog.outl("  %ins0.1 = extractelement {0} %ins0, i32 1".format(inType))
    cog.outl("  %ins1.1 = extractelement {0} %ins1, i32 1".format(inType))
    cog.outl("  %ret1 = tail call {0} @{1}({0} %ins0.1, {0} %ins1.1) nounwind readnone".format(callType, callName))
    # repack, and return
    cog.outl("  %rets0.0 = insertelement {0} undef, {1} %ret0, i32 0".format(inType, baseType))
    cog.outl("  %rets0.1 = insertelement {0} %rets0.0, {1} %ret1, i32 1".format(inType, baseType))
    cog.outl("  ret {0} %rets0.1".format(inType))
    cog.outl("}")
    cog.outl("")


def _getThunk_4to1_1in_1out(vecMathName, callName):
    """calls a packed func by unpacking a 2xdouble vector to singles"""
    raise NotImplementedError("Thunk not yet implemented")
#    inType = _getLLVMType(4)
#    callType = _getLLVMType(1)
#    baseType = _getLLVMType(1)
#    ## output the func definition
#    cog.outl("define {0} @{1}({0} %ins) nounwind alwaysinline readnone {{".format(inType, vecMathName))
#    cog.outl("entry:")
#    # unpack the ins and call
#    cog.outl("  %in0 = extractelement {0} %ins, i32 0".format(inType))
#    cog.outl("  %ret0 = tail call {0} @{1}({0} %in0) nounwind readnone".format(callType, callName))
#    cog.outl("  %in1 = extractelement {0} %ins, i32 1".format(inType))
#    cog.outl("  %ret1 = tail call {0} @{1}({0} %in1) nounwind readnone".format(callType, callName))
#    cog.outl("  %in2 = extractelement {0} %ins, i32 2".format(inType))
#    cog.outl("  %ret2 = tail call {0} @{1}({0} %in2) nounwind readnone".format(callType, callName))
#    cog.outl("  %in3 = extractelement {0} %ins, i32 3".format(inType))
#    cog.outl("  %ret3 = tail call {0} @{1}({0} %in3) nounwind readnone".format(callType, callName))
#    # repack, and return
#    cog.outl("  %retvec0.0 = insertelement {0} undef, {1} %ret0, i32 0".format(inType, baseType))
#    cog.outl("  %retvec0.1 = insertelement {0} %retvec0.0, {1} %ret1, i32 1".format(inType, baseType))
#    cog.outl("  %retvec0.2 = insertelement {0} %retvec0.1, {1} %ret2, i32 2".format(inType, baseType))
#    cog.outl("  %retvec0.3 = insertelement {0} %retvec0.2, {1} %ret3, i32 3".format(inType, baseType))
#    cog.outl("  ret {0} %retvec0.3".format(inType))
#    cog.outl("}")
#    cog.outl("")

def _getThunk_4to2_1in_1out(vecMathName, callName):
    """calls a packed func by unpacking a 2xdouble vector to singles"""
    raise NotImplementedError("Thunk not yet implemented")
#    inType = _getLLVMType(4)
#    callType = _getLLVMType(2)
#    baseType = _getLLVMType(1)
#    ## output the func definition
#    cog.outl("define {0} @{1}({0} %ins) nounwind alwaysinline readnone {{".format(inType, vecMathName))
#    cog.outl("entry:")
#
#    # extract 2 elems into a tmp vector and call
#    cog.outl("  %in0 = extractelement {0} %ins, i32 0".format(inType))
#    cog.outl("  %in1 = extractelement {0} %ins, i32 1".format(inType))
#    cog.outl("  %invec0.0 = insertelement {0} undef, {1} %in0, i32 0".format(callType, baseType))
#    cog.outl("  %invec0.1 = insertelement {0} %invec0.0, {1} %in1, i32 1".format(callType, baseType))
#    cog.outl("  %retvec0 = tail call {0} @{1}({0} %invec0.1) nounwind readnone".format(callType, callName))
#
#    # extract 2 elems into a tmp vector and call
#    cog.outl("  %in2 = extractelement {0} %ins, i32 0".format(inType))
#    cog.outl("  %in3 = extractelement {0} %ins, i32 1".format(inType))
#    cog.outl("  %invec1.0 = insertelement {0} undef, {1} %in2, i32 2".format(callType, baseType))
#    cog.outl("  %invec1.1 = insertelement {0} %invec1.0, {1} %in3, i32 3".format(callType, baseType))
#    cog.outl("  %retvec1 = tail call {0} @{1}({0} %invec1.1) nounwind readnone".format(callType, callName))
#
#    # unpack all vals, repack, and return
#    cog.outl("  %ret0 = extractelement {0} %retvec0, i32 0".format(callType))
#    cog.outl("  %ret1 = extractelement {0} %retvec0, i32 1".format(callType))
#    cog.outl("  %ret2 = extractelement {0} %retvec1, i32 0".format(callType))
#    cog.outl("  %ret3 = extractelement {0} %retvec1, i32 1".format(callType))
#    cog.outl("  %retvec2.0 = insertelement {0} undef, {1} %ret0, i32 0".format(inType, baseType))
#    cog.outl("  %retvec2.1 = insertelement {0} %retvec2.0, {1} %ret1, i32 1".format(inType, baseType))
#    cog.outl("  %retvec2.2 = insertelement {0} %retvec2.1, {1} %ret2, i32 2".format(inType, baseType))
#    cog.outl("  %retvec2.3 = insertelement {0} %retvec2.2, {1} %ret3, i32 3".format(inType, baseType))
#    cog.outl("  ret {0} %retvec2.3".format(inType))
#    cog.outl("}")
#    cog.outl("")
