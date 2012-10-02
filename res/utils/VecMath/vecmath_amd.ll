;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VecMath Wrapper onto AMD libM library (this applies partial (2xDouble) vectorisation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generate prototypes
;;[[[cog 
;;  import cog
;;  import VecMathCog as C
;;  # amd libM supported vector functions
;;  # also 'sincos', 'pow'
;;  amdFuncsFtoF =  { 'exp', 'exp2', 'exp10', 'expm1'
;;                  , 'log', 'log2', 'log10', 'log1p'
;;                  , 'cosh', 'cos', 'sin', 'tan'
;;                  , 'cbrt'
;;                  }
;;
;;  # scalar prototypes
;;  vecSize = 1
;;  for f in C.funcsFtoF:
;;    protoName = C.getCallName(f, vecSize, C.MathLib.AMD)
;;    C.genProto(protoName, vecSize)
;;  # AMD only supports vecx2
;;  vecSize = 2
;;  for f in amdFuncsFtoF:
;;    protoName = C.getCallName(f, vecSize, C.MathLib.AMD)
;;    C.genProto(protoName, vecSize)
;;]]]
;;[[[end]]]


;;[[[cog 
;;  # scalar thunks
;;  vecSize = 1 # = callVecSize
;;  for f in C.funcsFtoF:
;;    callName = C.getCallName(f, vecSize, C.MathLib.AMD)
;;    C.genThunk(f, callName, vecSize, vecSize)
;;]]]
;;[[[end]]]


;;[[[cog 
;;  # vec2 thunks
;;  vecSize = 2 # = callVecSize
;;  for f in amdFuncsFtoF:
;;    callName = C.getCallName(f, vecSize, C.MathLib.AMD)
;;    C.genThunk(f, callName, vecSize, vecSize)
;;  # have to unpack the remaining funcs
;;  callVecSize = 1
;;  for f in (C.funcsFtoF - amdFuncsFtoF):
;;    callName = C.getCallName(f, callVecSize, C.MathLib.AMD)
;;    C.genThunk(f, callName, vecSize, callVecSize)
;;]]]
;;[[[end]]]

;;[[[cog 
;;  # vec4 thunks
;;  vecSize = 4
;;  callVecSize = 2
;;  for f in amdFuncsFtoF:
;;    callName = C.getCallName(f, callVecSize, C.MathLib.AMD)
;;    C.genThunk(f, callName, vecSize, callVecSize)
;;  # have to unpack the remaining funcs
;;  callVecSize = 1
;;  for f in (C.funcsFtoF - amdFuncsFtoF):
;;    callName = C.getCallName(f, callVecSize, C.MathLib.AMD)
;;    C.genThunk(f, callName, vecSize, callVecSize)
;;]]]
;;[[[end]]]

