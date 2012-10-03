;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VecMath Wrapper onto GNU libM library
;; - (this doesnt apply any vectorisation)
;; - only valid for vecsize = {1,2}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generate prototypes
;;[[[cog 
;;  import cog
;;  import VecMathCog as C
;;  # libm funcs with finiste wrappers
;;  libmFinites =   { 'acos', 'acosh', 'asin', 'atan2', 'atanh', 'cosh', 'sinh'
;;                  , 'exp10', 'exp2', 'exp', 'log10', 'log2', 'log'
;;                  , 'fmod', 'hypot', 'pow', 'sqrt'
;;                  }
;;
;;  # get the correct lib call name
;;  def getCallName(funcName, vecSize=1):
;;      return "__{0}_finite".format(funcName) if funcName in libmFinites else funcName
;;
;;  # scalar prototypes only
;;  callVecSize = 1
;;  for f in C.funcsFtoF:
;;    C.genProto(getCallName(f), callVecSize)
;;  for f in C.funcsFFtoF:
;;    C.genProto(getCallName(f), callVecSize, 2)
;;]]]
;;[[[end]]]


;;[[[cog 
;;  # scalar thunks
;;  inVecSize = callVecSize = 1
;;  for f in C.funcsFtoF:
;;    C.genThunk(f, getCallName(f), inVecSize, callVecSize)
;;  for f in C.funcsFFtoF:
;;    C.genThunk(f, getCallName(f), inVecSize, callVecSize, 2)
;;]]]
;;[[[end]]]


;;[[[cog 
;;  # vec2 thunks
;;  inVecSize = 2
;;  callVecSize = 1
;;  for f in C.funcsFtoF:
;;    C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize)
;;  for f in C.funcsFFtoF:
;;    C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize, 2)
;;]]]
;;[[[end]]]

