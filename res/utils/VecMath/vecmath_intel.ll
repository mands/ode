;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VecMath Wrapper onto Intel SVML library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generate prototypes
;;[[[cog 
;;  import cog
;;  import VecMathCog as C
;;  # scalar prototypes
;;  vecSize = 1
;;  for f in C.funcsFtoF:
;;    protoName = C.getCallName(f, vecSize, C.MathLib.Intel)
;;    C.genProto(protoName, vecSize) 
;;  # svml prototypes
;;  vecSize = 2
;;  for f in C.funcsFtoF:
;;    protoName = C.getCallName(f, vecSize, C.MathLib.Intel)
;;    C.genProto(protoName, vecSize)
;;  # svml prototypes
;;  vecSize = 4
;;  for f in C.funcsFtoF:
;;    protoName = C.getCallName(f, vecSize, C.MathLib.Intel)
;;    C.genProto(protoName, vecSize)
;;  
;;]]]
;;[[[end]]]


;;[[[cog 
;;  # scalar thunks
;;  vecSize = 1 # = callVecSize
;;  for f in C.funcsFtoF:
;;    callName = C.getCallName(f, vecSize, C.MathLib.Intel)
;;    C.genThunk(f, callName, vecSize, vecSize)
;;]]]
;;[[[end]]]


;;[[[cog 
;;  # svml thunks
;;  vecSize = 2 # = callVecSize
;;  for f in C.funcsFtoF:
;;    callName = C.getCallName(f, vecSize, C.MathLib.Intel)
;;    C.genThunk(f, callName, vecSize, vecSize)
;;]]]
;;[[[end]]]

;;[[[cog 
;;  # svml thunks
;;  vecSize = 4 # = callVecSize
;;  for f in C.funcsFtoF:
;;    callName = C.getCallName(f, vecSize, C.MathLib.Intel)
;;    C.genThunk(f, callName, vecSize, vecSize)
;;]]]
;;[[[end]]]

