;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VecMath Wrapper onto Intel SVML library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generate prototypes
;;[[[cog 
;;  import cog
;;  import VecMathCog as C
;;
;;  # get the correct lib call name
;;  def getCallName(funcName, vecSize=1):
;;      return funcName if vecSize == 1 else "__svml_{0}{1}".format(funcName, vecSize)
;;
;;  # scalar prototypes
;;  callVecSize = 1
;;  for f in C.funcsFtoF:
;;      C.genProto(getCallName(f), callVecSize) 
;;  for f in C.funcsFFtoF:
;;      C.genProto(getCallName(f), callVecSize, 2)
;;  # svml prototypes
;;  callVecSize = 2
;;  for f in C.funcsFtoF:
;;      C.genProto(getCallName(f, callVecSize), callVecSize)
;;  for f in C.funcsFFtoF:
;;      C.genProto(getCallName(f, callVecSize), callVecSize, 2)
;;  # svml prototypes
;;  callVecSize = 4
;;  for f in C.funcsFtoF:
;;      C.genProto(getCallName(f, callVecSize), callVecSize)
;;  for f in C.funcsFFtoF:
;;      C.genProto(getCallName(f, callVecSize), callVecSize, 2)
;;
;;]]]
;;[[[end]]]


;;[[[cog 
;;  # scalar thunks
;;  inVecSize = callVecSize = 1
;;  for f in C.funcsFtoF:
;;      C.genThunk(f, getCallName(f), inVecSize, callVecSize)
;;  for f in C.funcsFFtoF:
;;      C.genThunk(f, getCallName(f), inVecSize, callVecSize, 2)
;;]]]
;;[[[end]]]


;;[[[cog 
;;  # svml thunks
;;  inVecSize = callVecSize = 2
;;  for f in C.funcsFtoF:
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize)
;;  for f in C.funcsFFtoF:
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize, 2)
;;]]]
;;[[[end]]]

;;[[[cog 
;;  # svml thunks
;;  inVecSize = callVecSize = 4
;;  for f in C.funcsFtoF:
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize)
;;  for f in C.funcsFFtoF:
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize, 2)
;;]]]
;;[[[end]]]

