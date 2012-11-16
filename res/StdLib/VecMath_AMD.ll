target triple = "x86_64-unknown-linux-gnu"

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
declare double @amd_cbrt(double %ins) nounwind readnone
declare double @amd_cosh(double %ins) nounwind readnone
declare double @amd_logb(double %ins) nounwind readnone
declare double @amd_acosh(double %ins) nounwind readnone
declare double @amd_tan(double %ins) nounwind readnone
declare double @amd_asin(double %ins) nounwind readnone
declare double @amd_log(double %ins) nounwind readnone
declare double @amd_exp2(double %ins) nounwind readnone
declare double @amd_atanh(double %ins) nounwind readnone
declare double @amd_sqrt(double %ins) nounwind readnone
declare double @amd_log10(double %ins) nounwind readnone
declare double @amd_sin(double %ins) nounwind readnone
declare double @amd_exp10(double %ins) nounwind readnone
declare double @amd_asinh(double %ins) nounwind readnone
declare double @amd_log2(double %ins) nounwind readnone
declare double @amd_atan(double %ins) nounwind readnone
declare double @amd_sinh(double %ins) nounwind readnone
declare double @amd_cos(double %ins) nounwind readnone
declare double @amd_tanh(double %ins) nounwind readnone
declare double @amd_erf(double %ins) nounwind readnone
declare double @amd_erfc(double %ins) nounwind readnone
declare double @amd_exp(double %ins) nounwind readnone
declare double @amd_pow10(double %ins) nounwind readnone
declare double @amd_acos(double %ins) nounwind readnone
declare double @amd_pow(double %ins, double %ins2) nounwind readnone
declare double @amd_hypot(double %ins, double %ins2) nounwind readnone
declare double @amd_atan2(double %ins, double %ins2) nounwind readnone
declare <2 x double> @amd_vrd2_cos(<2 x double> %ins) nounwind readnone
declare <2 x double> @amd_vrd2_log2(<2 x double> %ins) nounwind readnone
declare <2 x double> @amd_vrd2_log(<2 x double> %ins) nounwind readnone
declare <2 x double> @amd_vrd2_log10(<2 x double> %ins) nounwind readnone
declare <2 x double> @amd_vrd2_cbrt(<2 x double> %ins) nounwind readnone
declare <2 x double> @amd_vrd2_expm1(<2 x double> %ins) nounwind readnone
declare <2 x double> @amd_vrd2_cosh(<2 x double> %ins) nounwind readnone
declare <2 x double> @amd_vrd2_exp(<2 x double> %ins) nounwind readnone
declare <2 x double> @amd_vrd2_exp2(<2 x double> %ins) nounwind readnone
declare <2 x double> @amd_vrd2_tan(<2 x double> %ins) nounwind readnone
declare <2 x double> @amd_vrd2_log1p(<2 x double> %ins) nounwind readnone
declare <2 x double> @amd_vrd2_sin(<2 x double> %ins) nounwind readnone
declare <2 x double> @amd_vrd2_exp10(<2 x double> %ins) nounwind readnone
declare <2 x double> @amd_vrd2_pow(<2 x double> %ins, <2 x double> %ins2) nounwind readnone
;;[[[end]]] (checksum: c0f90d14c07522deba4d34ab07f019f6)


;;[[[cog 
;;  # scalar thunks
;;  inVecSize = callVecSize = 1
;;  for f in C.funcsFtoF:
;;      C.genThunk(f, getCallName(f), inVecSize, callVecSize)
;;  for f in C.funcsFFtoF:
;;      C.genThunk(f, getCallName(f), inVecSize, callVecSize, 2)
;;]]]
define double @vecmath_cbrt_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_cbrt(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_cosh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_cosh(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_logb_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_logb(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_acosh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_acosh(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_tan_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_tan(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_asin_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_asin(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_log_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_log(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_exp2_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_exp2(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_atanh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_atanh(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_sqrt_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_sqrt(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_log10_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_log10(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_sin_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_sin(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_exp10_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_exp10(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_asinh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_asinh(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_log2_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_log2(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_atan_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_atan(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_sinh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_sinh(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_cos_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_cos(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_tanh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_tanh(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_erf_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_erf(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_erfc_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_erfc(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_exp_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_exp(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_pow10_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_pow10(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_acos_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_acos(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_pow_f64(double %ins0, double %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_pow(double %ins0, double %ins1) nounwind readnone
  ret double %rets0
}

define double @vecmath_hypot_f64(double %ins0, double %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_hypot(double %ins0, double %ins1) nounwind readnone
  ret double %rets0
}

define double @vecmath_atan2_f64(double %ins0, double %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @amd_atan2(double %ins0, double %ins1) nounwind readnone
  ret double %rets0
}

;;[[[end]]] (checksum: 8b5ad914d52f06ee63a1769c8e6e9941)


;;[[[cog 
;;  # vec2 thunks
;;  inVecSize = 2
;;  callVecSize = 1
;;  # have to unpack un-supported funcs
;;  for f in (C.funcsFtoF - amdFuncsFtoF):
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize)
;;  for f in (C.funcsFFtoF - amdFuncsFFtoF):
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize, 2)
;;  # AMD libM supported funcs
;;  callVecSize = 2
;;  for f in amdFuncsFtoF:
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize)
;;  for f in amdFuncsFFtoF:
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize, 2)
;;]]]
define <2 x double> @vecmath_asin_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @amd_asin(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @amd_asin(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_asinh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @amd_asinh(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @amd_asinh(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_atan_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @amd_atan(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @amd_atan(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_tanh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @amd_tanh(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @amd_tanh(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_atanh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @amd_atanh(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @amd_atanh(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_sqrt_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @amd_sqrt(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @amd_sqrt(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_erf_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @amd_erf(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @amd_erf(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_logb_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @amd_logb(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @amd_logb(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_sinh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @amd_sinh(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @amd_sinh(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_acosh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @amd_acosh(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @amd_acosh(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_pow10_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @amd_pow10(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @amd_pow10(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_erfc_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @amd_erfc(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @amd_erfc(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_acos_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @amd_acos(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @amd_acos(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_atan2_v2f64(<2 x double> %ins0, <2 x double> %ins1) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ins1.0 = extractelement <2 x double> %ins1, i32 0
  %ret0 = tail call fastcc double @amd_atan2(double %ins0.0, double %ins1.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ins1.1 = extractelement <2 x double> %ins1, i32 1
  %ret1 = tail call fastcc double @amd_atan2(double %ins0.1, double %ins1.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_hypot_v2f64(<2 x double> %ins0, <2 x double> %ins1) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ins1.0 = extractelement <2 x double> %ins1, i32 0
  %ret0 = tail call fastcc double @amd_hypot(double %ins0.0, double %ins1.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ins1.1 = extractelement <2 x double> %ins1, i32 1
  %ret1 = tail call fastcc double @amd_hypot(double %ins0.1, double %ins1.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_cos_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_cos(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_log2_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_log2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_log_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_log(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_log10_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_log10(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_cbrt_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_cbrt(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_expm1_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_expm1(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_cosh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_cosh(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_exp_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_exp(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_exp2_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_exp2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_tan_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_tan(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_log1p_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_log1p(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_sin_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_sin(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_exp10_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_exp10(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_pow_v2f64(<2 x double> %ins0, <2 x double> %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @amd_vrd2_pow(<2 x double> %ins0, <2 x double> %ins1) nounwind readnone
  ret <2 x double> %rets0
}

;;[[[end]]] (checksum: c19f8676b5d4932b4a05b324fdbfc8f5)

