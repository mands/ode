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
declare double @cbrt(double %ins) nounwind readnone
declare double @cosh(double %ins) nounwind readnone
declare double @logb(double %ins) nounwind readnone
declare double @acosh(double %ins) nounwind readnone
declare double @tan(double %ins) nounwind readnone
declare double @asin(double %ins) nounwind readnone
declare double @log(double %ins) nounwind readnone
declare double @exp2(double %ins) nounwind readnone
declare double @atanh(double %ins) nounwind readnone
declare double @sqrt(double %ins) nounwind readnone
declare double @log10(double %ins) nounwind readnone
declare double @sin(double %ins) nounwind readnone
declare double @exp10(double %ins) nounwind readnone
declare double @asinh(double %ins) nounwind readnone
declare double @log2(double %ins) nounwind readnone
declare double @atan(double %ins) nounwind readnone
declare double @sinh(double %ins) nounwind readnone
declare double @cos(double %ins) nounwind readnone
declare double @tanh(double %ins) nounwind readnone
declare double @erf(double %ins) nounwind readnone
declare double @erfc(double %ins) nounwind readnone
declare double @exp(double %ins) nounwind readnone
declare double @pow10(double %ins) nounwind readnone
declare double @acos(double %ins) nounwind readnone
declare double @pow(double %ins, double %ins2) nounwind readnone
declare double @hypot(double %ins, double %ins2) nounwind readnone
declare double @atan2(double %ins, double %ins2) nounwind readnone
declare <2 x double> @__svml_cbrt2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_cosh2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_logb2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_acosh2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_tan2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_asin2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_log2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_exp22(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_atanh2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_sqrt2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_log102(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_sin2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_exp102(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_asinh2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_log22(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_atan2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_sinh2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_cos2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_tanh2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_erf2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_erfc2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_exp2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_pow102(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_acos2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__svml_pow2(<2 x double> %ins, <2 x double> %ins2) nounwind readnone
declare <2 x double> @__svml_hypot2(<2 x double> %ins, <2 x double> %ins2) nounwind readnone
declare <2 x double> @__svml_atan22(<2 x double> %ins, <2 x double> %ins2) nounwind readnone
declare <4 x double> @__svml_cbrt4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_cosh4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_logb4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_acosh4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_tan4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_asin4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_log4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_exp24(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_atanh4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_sqrt4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_log104(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_sin4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_exp104(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_asinh4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_log24(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_atan4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_sinh4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_cos4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_tanh4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_erf4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_erfc4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_exp4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_pow104(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_acos4(<4 x double> %ins) nounwind readnone
declare <4 x double> @__svml_pow4(<4 x double> %ins, <4 x double> %ins2) nounwind readnone
declare <4 x double> @__svml_hypot4(<4 x double> %ins, <4 x double> %ins2) nounwind readnone
declare <4 x double> @__svml_atan24(<4 x double> %ins, <4 x double> %ins2) nounwind readnone
;;[[[end]]] (checksum: fb3d977a48d6331772fb41b7c0c6a232)


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
  %rets0 = tail call fastcc double @cbrt(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_cosh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @cosh(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_logb_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @logb(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_acosh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @acosh(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_tan_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @tan(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_asin_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @asin(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_log_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @log(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_exp2_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @exp2(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_atanh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @atanh(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_sqrt_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @sqrt(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_log10_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @log10(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_sin_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @sin(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_exp10_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @exp10(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_asinh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @asinh(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_log2_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @log2(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_atan_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @atan(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_sinh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @sinh(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_cos_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @cos(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_tanh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @tanh(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_erf_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @erf(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_erfc_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @erfc(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_exp_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @exp(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_pow10_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @pow10(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_acos_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @acos(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_pow_f64(double %ins0, double %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @pow(double %ins0, double %ins1) nounwind readnone
  ret double %rets0
}

define double @vecmath_hypot_f64(double %ins0, double %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @hypot(double %ins0, double %ins1) nounwind readnone
  ret double %rets0
}

define double @vecmath_atan2_f64(double %ins0, double %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @atan2(double %ins0, double %ins1) nounwind readnone
  ret double %rets0
}

;;[[[end]]] (checksum: ae3f7ee6ed92d47e35f5a29cb4275177)


;;[[[cog 
;;  # svml thunks
;;  inVecSize = callVecSize = 2
;;  for f in C.funcsFtoF:
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize)
;;  for f in C.funcsFFtoF:
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize, 2)
;;]]]
define <2 x double> @vecmath_cbrt_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_cbrt2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_cosh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_cosh2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_logb_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_logb2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_acosh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_acosh2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_tan_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_tan2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_asin_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_asin2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_log_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_log2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_exp2_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_exp22(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_atanh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_atanh2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_sqrt_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_sqrt2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_log10_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_log102(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_sin_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_sin2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_exp10_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_exp102(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_asinh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_asinh2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_log2_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_log22(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_atan_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_atan2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_sinh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_sinh2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_cos_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_cos2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_tanh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_tanh2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_erf_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_erf2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_erfc_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_erfc2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_exp_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_exp2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_pow10_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_pow102(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_acos_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_acos2(<2 x double> %ins0) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_pow_v2f64(<2 x double> %ins0, <2 x double> %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_pow2(<2 x double> %ins0, <2 x double> %ins1) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_hypot_v2f64(<2 x double> %ins0, <2 x double> %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_hypot2(<2 x double> %ins0, <2 x double> %ins1) nounwind readnone
  ret <2 x double> %rets0
}

define <2 x double> @vecmath_atan2_v2f64(<2 x double> %ins0, <2 x double> %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <2 x double> @__svml_atan22(<2 x double> %ins0, <2 x double> %ins1) nounwind readnone
  ret <2 x double> %rets0
}

;;[[[end]]] (checksum: a843b24af23fa5c20d1b5bf112704c6f)

;;[[[cog 
;;  # svml thunks
;;  inVecSize = callVecSize = 4
;;  for f in C.funcsFtoF:
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize)
;;  for f in C.funcsFFtoF:
;;      C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize, 2)
;;]]]
define <4 x double> @vecmath_cbrt_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_cbrt4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_cosh_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_cosh4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_logb_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_logb4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_acosh_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_acosh4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_tan_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_tan4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_asin_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_asin4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_log_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_log4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_exp2_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_exp24(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_atanh_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_atanh4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_sqrt_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_sqrt4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_log10_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_log104(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_sin_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_sin4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_exp10_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_exp104(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_asinh_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_asinh4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_log2_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_log24(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_atan_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_atan4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_sinh_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_sinh4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_cos_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_cos4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_tanh_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_tanh4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_erf_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_erf4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_erfc_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_erfc4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_exp_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_exp4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_pow10_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_pow104(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_acos_v4f64(<4 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_acos4(<4 x double> %ins0) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_pow_v4f64(<4 x double> %ins0, <4 x double> %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_pow4(<4 x double> %ins0, <4 x double> %ins1) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_hypot_v4f64(<4 x double> %ins0, <4 x double> %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_hypot4(<4 x double> %ins0, <4 x double> %ins1) nounwind readnone
  ret <4 x double> %rets0
}

define <4 x double> @vecmath_atan2_v4f64(<4 x double> %ins0, <4 x double> %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc <4 x double> @__svml_atan24(<4 x double> %ins0, <4 x double> %ins1) nounwind readnone
  ret <4 x double> %rets0
}

;;[[[end]]] (checksum: ad545f3ec1729b42a3ebde03c03cf2b0)

