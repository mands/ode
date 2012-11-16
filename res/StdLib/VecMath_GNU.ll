target triple = "x86_64-unknown-linux-gnu"

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
declare double @cbrt(double %ins) nounwind readnone
declare double @__cosh_finite(double %ins) nounwind readnone
declare double @logb(double %ins) nounwind readnone
declare double @__acosh_finite(double %ins) nounwind readnone
declare double @tan(double %ins) nounwind readnone
declare double @__asin_finite(double %ins) nounwind readnone
declare double @__log_finite(double %ins) nounwind readnone
declare double @__exp2_finite(double %ins) nounwind readnone
declare double @__atanh_finite(double %ins) nounwind readnone
declare double @__sqrt_finite(double %ins) nounwind readnone
declare double @__log10_finite(double %ins) nounwind readnone
declare double @sin(double %ins) nounwind readnone
declare double @__exp10_finite(double %ins) nounwind readnone
declare double @asinh(double %ins) nounwind readnone
declare double @__log2_finite(double %ins) nounwind readnone
declare double @atan(double %ins) nounwind readnone
declare double @__sinh_finite(double %ins) nounwind readnone
declare double @cos(double %ins) nounwind readnone
declare double @tanh(double %ins) nounwind readnone
declare double @erf(double %ins) nounwind readnone
declare double @erfc(double %ins) nounwind readnone
declare double @__exp_finite(double %ins) nounwind readnone
declare double @pow10(double %ins) nounwind readnone
declare double @__acos_finite(double %ins) nounwind readnone
declare double @__pow_finite(double %ins, double %ins2) nounwind readnone
declare double @__hypot_finite(double %ins, double %ins2) nounwind readnone
declare double @__atan2_finite(double %ins, double %ins2) nounwind readnone
;;[[[end]]] (checksum: a129d17038e763a351466ccf311a75e4)


;;[[[cog 
;;  # scalar thunks
;;  inVecSize = callVecSize = 1
;;  for f in C.funcsFtoF:
;;    C.genThunk(f, getCallName(f), inVecSize, callVecSize)
;;  for f in C.funcsFFtoF:
;;    C.genThunk(f, getCallName(f), inVecSize, callVecSize, 2)
;;]]]
define double @vecmath_cbrt_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @cbrt(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_cosh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__cosh_finite(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_logb_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @logb(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_acosh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__acosh_finite(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_tan_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @tan(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_asin_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__asin_finite(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_log_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__log_finite(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_exp2_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__exp2_finite(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_atanh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__atanh_finite(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_sqrt_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__sqrt_finite(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_log10_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__log10_finite(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_sin_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @sin(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_exp10_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__exp10_finite(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_asinh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @asinh(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_log2_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__log2_finite(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_atan_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @atan(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_sinh_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__sinh_finite(double %ins0) nounwind readnone
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
  %rets0 = tail call fastcc double @__exp_finite(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_pow10_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @pow10(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_acos_f64(double %ins0) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__acos_finite(double %ins0) nounwind readnone
  ret double %rets0
}

define double @vecmath_pow_f64(double %ins0, double %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__pow_finite(double %ins0, double %ins1) nounwind readnone
  ret double %rets0
}

define double @vecmath_hypot_f64(double %ins0, double %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__hypot_finite(double %ins0, double %ins1) nounwind readnone
  ret double %rets0
}

define double @vecmath_atan2_f64(double %ins0, double %ins1) nounwind alwaysinline readnone {
entry:
  %rets0 = tail call fastcc double @__atan2_finite(double %ins0, double %ins1) nounwind readnone
  ret double %rets0
}

;;[[[end]]] (checksum: ac48c2d81a90e10caae99e767a210041)


;;[[[cog 
;;  # vec2 thunks
;;  inVecSize = 2
;;  callVecSize = 1
;;  for f in C.funcsFtoF:
;;    C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize)
;;  for f in C.funcsFFtoF:
;;    C.genThunk(f, getCallName(f, callVecSize), inVecSize, callVecSize, 2)
;;]]]
define <2 x double> @vecmath_cbrt_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @cbrt(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @cbrt(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_cosh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @__cosh_finite(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @__cosh_finite(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_logb_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @logb(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @logb(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_acosh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @__acosh_finite(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @__acosh_finite(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_tan_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @tan(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @tan(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_asin_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @__asin_finite(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @__asin_finite(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_log_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @__log_finite(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @__log_finite(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_exp2_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @__exp2_finite(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @__exp2_finite(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_atanh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @__atanh_finite(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @__atanh_finite(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_sqrt_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @__sqrt_finite(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @__sqrt_finite(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_log10_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @__log10_finite(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @__log10_finite(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_sin_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @sin(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @sin(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_exp10_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @__exp10_finite(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @__exp10_finite(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_asinh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @asinh(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @asinh(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_log2_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @__log2_finite(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @__log2_finite(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_atan_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @atan(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @atan(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_sinh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @__sinh_finite(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @__sinh_finite(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_cos_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @cos(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @cos(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_tanh_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @tanh(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @tanh(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_erf_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @erf(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @erf(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_erfc_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @erfc(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @erfc(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_exp_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @__exp_finite(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @__exp_finite(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_pow10_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @pow10(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @pow10(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_acos_v2f64(<2 x double> %ins0) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ret0 = tail call fastcc double @__acos_finite(double %ins0.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ret1 = tail call fastcc double @__acos_finite(double %ins0.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_pow_v2f64(<2 x double> %ins0, <2 x double> %ins1) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ins1.0 = extractelement <2 x double> %ins1, i32 0
  %ret0 = tail call fastcc double @__pow_finite(double %ins0.0, double %ins1.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ins1.1 = extractelement <2 x double> %ins1, i32 1
  %ret1 = tail call fastcc double @__pow_finite(double %ins0.1, double %ins1.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_hypot_v2f64(<2 x double> %ins0, <2 x double> %ins1) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ins1.0 = extractelement <2 x double> %ins1, i32 0
  %ret0 = tail call fastcc double @__hypot_finite(double %ins0.0, double %ins1.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ins1.1 = extractelement <2 x double> %ins1, i32 1
  %ret1 = tail call fastcc double @__hypot_finite(double %ins0.1, double %ins1.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

define <2 x double> @vecmath_atan2_v2f64(<2 x double> %ins0, <2 x double> %ins1) nounwind alwaysinline readnone {
entry:
  %ins0.0 = extractelement <2 x double> %ins0, i32 0
  %ins1.0 = extractelement <2 x double> %ins1, i32 0
  %ret0 = tail call fastcc double @__atan2_finite(double %ins0.0, double %ins1.0) nounwind readnone
  %ins0.1 = extractelement <2 x double> %ins0, i32 1
  %ins1.1 = extractelement <2 x double> %ins1, i32 1
  %ret1 = tail call fastcc double @__atan2_finite(double %ins0.1, double %ins1.1) nounwind readnone
  %rets0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %rets0.1 = insertelement <2 x double> %rets0.0, double %ret1, i32 1
  ret <2 x double> %rets0.1
}

;;[[[end]]] (checksum: aa80575a9d3ce1ff02cde33f631486cc)

