;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VecMath Wrapper onto AMD libM library (this applies partial (2xDouble) vectorisation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generate prototypes
;;[[[cog 
;;  import cog
;;  import VecMathCog as C
;;  # amd libM supported vector functions
;;  amdFuncsFtoF =  { 'exp', 'exp2', 'exp10', 'expm1'
;;                  , 'log', 'log2', 'log10', 'log1p'
;;                  , 'cosh', 'cos', 'sin', 'tan'
;;                  , 'cbrt'
;;                  }
;;  amdFuncsFFtoF =  { 'pow' }
;;
;;  # get the correct lib call name
;;  def getCallName(funcName, vecSize=1):
;;      if vecSize == 1:
;;          return "amd_{0}".format(funcName)
;;      else:
;;          return "amd_vrd{1}_{0}".format(funcName, vecSize)
;;
;;  # scalar prototypes
;;  callVecSize = 1
;;  for f in C.funcsFtoF:
;;      C.genProto(getCallName(f), callVecSize)
;;  # scalar prototypes
;;  for f in C.funcsFFtoF:
;;      C.genProto(getCallName(f), callVecSize, 2)
;;  # AMD only supports vecx2
;;  callVecSize = 2
;;  for f in amdFuncsFtoF:
;;    C.genProto(getCallName(f, callVecSize), callVecSize)
;;  # AMD only supports vecx2
;;  for f in amdFuncsFFtoF:
;;    C.genProto(getCallName(f, callVecSize), callVecSize, 2)
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
;;  # vec2 thunks
;;  inVecSize = callVecSize = 2
;;  for f in amdFuncsFtoF:
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize)
;;  for f in amdFuncsFFtoF:
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize, 2)
;;  # have to unpack the remaining funcs
;;  callVecSize = 1
;;  for f in (C.funcsFtoF - amdFuncsFtoF):
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize)
;;  for f in (C.funcsFFtoF - amdFuncsFFtoF):
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize, 2)
;;]]]
;;[[[end]]]

