;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VecMath Wrapper onto GNU libM library (this doesnt apply any vectorisation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generate prototypes
;;[[[cog 
;;  import cog
;;  import VecMathCog as C
;;  # scalar prototypes only
;;  vecSize = 1
;;  for f in C.funcsFtoF:
;;    protoName = C.getCallName(f, vecSize, C.MathLib.GNU)
;;    C.genProto(protoName, vecSize)
;;]]]
;;[[[end]]]


;;[[[cog 
;;  # scalar thunks
;;  vecSize = 1 # = callVecSize
;;  for f in C.funcsFtoF:
;;    callName = C.getCallName(f, vecSize, C.MathLib.GNU)
;;    C.genThunk(f, callName, vecSize, 1)
;;]]]
;;[[[end]]]


;;[[[cog 
;;  # vec2 thunks
;;  vecSize = 2
;;  callVecSize = 1
;;  for f in C.funcsFtoF:
;;    callName = C.getCallName(f, callVecSize, C.MathLib.GNU)
;;    C.genThunk(f, callName, vecSize, callVecSize)
;;]]]
;;[[[end]]]

;;[[[cog 
;;  # vec4 thunks
;;  vecSize = 4
;;  callVecSize = 1
;;  for f in C.funcsFtoF:
;;    callName = C.getCallName(f, callVecSize, C.MathLib.GNU)
;;    C.genThunk(f, callName, vecSize, callVecSize)
;;]]]
;;[[[end]]]

