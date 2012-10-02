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
;;                  , 'cosh', 'cos', 'sin'
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
declare double @cos(double %ins) nounwind readnone
declare double @sinh(double %ins) nounwind readnone
declare double @expm1(double %ins) nounwind readnone
declare double @tanh(double %ins) nounwind readnone
declare double @erf(double %ins) nounwind readnone
declare double @erfc(double %ins) nounwind readnone
declare double @exp(double %ins) nounwind readnone
declare double @pow10(double %ins) nounwind readnone
declare double @acos(double %ins) nounwind readnone
declare double @log1p(double %ins) nounwind readnone
declare <2 x double> @__vrd2_cos(<2 x double> %ins) nounwind readnone
declare <2 x double> @__vrd2_log2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__vrd2_log1p(<2 x double> %ins) nounwind readnone
declare <2 x double> @__vrd2_cbrt(<2 x double> %ins) nounwind readnone
declare <2 x double> @__vrd2_expm1(<2 x double> %ins) nounwind readnone
declare <2 x double> @__vrd2_cosh(<2 x double> %ins) nounwind readnone
declare <2 x double> @__vrd2_exp(<2 x double> %ins) nounwind readnone
declare <2 x double> @__vrd2_exp2(<2 x double> %ins) nounwind readnone
declare <2 x double> @__vrd2_exp10(<2 x double> %ins) nounwind readnone
declare <2 x double> @__vrd2_log10(<2 x double> %ins) nounwind readnone
declare <2 x double> @__vrd2_sin(<2 x double> %ins) nounwind readnone
declare <2 x double> @__vrd2_log(<2 x double> %ins) nounwind readnone
;;[[[end]]]


;;[[[cog 
;;  # scalar thunks
;;  vecSize = 1 # = callVecSize
;;  for f in C.funcsFtoF:
;;    callName = C.getCallName(f, vecSize, C.MathLib.AMD)
;;    C.genThunk(f, callName, vecSize, vecSize)
;;]]]
define double @vecmath_cbrt_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @cbrt(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_cosh_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @cosh(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_logb_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @logb(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_acosh_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @acosh(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_tan_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @tan(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_asin_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @asin(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_log_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @log(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_exp2_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @exp2(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_atanh_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @atanh(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_sqrt_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @sqrt(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_log10_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @log10(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_sin_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @sin(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_exp10_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @exp10(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_asinh_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @asinh(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_log2_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @log2(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_atan_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @atan(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_cos_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @cos(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_sinh_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @sinh(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_expm1_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @expm1(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_tanh_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @tanh(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_erf_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @erf(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_erfc_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @erfc(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_exp_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @exp(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_pow10_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @pow10(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_acos_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @acos(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_log1p_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @log1p(double %ins) nounwind readnone
  ret double %ret0
}

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
define <2 x double> @vecmath_cos_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call <2 x double> @__vrd2_cos(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_log2_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call <2 x double> @__vrd2_log2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_log1p_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call <2 x double> @__vrd2_log1p(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_cbrt_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call <2 x double> @__vrd2_cbrt(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_expm1_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call <2 x double> @__vrd2_expm1(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_cosh_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call <2 x double> @__vrd2_cosh(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_exp_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call <2 x double> @__vrd2_exp(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_exp2_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call <2 x double> @__vrd2_exp2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_exp10_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call <2 x double> @__vrd2_exp10(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_log10_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call <2 x double> @__vrd2_log10(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_sin_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call <2 x double> @__vrd2_sin(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_log_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call <2 x double> @__vrd2_log(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_asin_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @asin(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @asin(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_asinh_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @asinh(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @asinh(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_atan_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @atan(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @atan(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_tanh_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @tanh(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @tanh(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_atanh_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @atanh(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @atanh(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_sqrt_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @sqrt(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @sqrt(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_erf_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @erf(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @erf(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_logb_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @logb(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @logb(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_sinh_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @sinh(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @sinh(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_acosh_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @acosh(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @acosh(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_pow10_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @pow10(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @pow10(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_erfc_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @erfc(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @erfc(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_tan_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @tan(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @tan(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_acos_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @acos(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @acos(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

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
define <4 x double> @vecmath_cos_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %in1 = extractelement <4 x double> %ins, i32 1
  %invec0.0 = insertelement <2 x double> undef, double %in0, i32 0
  %invec0.1 = insertelement <2 x double> %invec0.0, double %in1, i32 1
  %retvec0 = tail call <2 x double> @__vrd2_cos(<2 x double> %invec0.1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 0
  %in3 = extractelement <4 x double> %ins, i32 1
  %invec1.0 = insertelement <2 x double> undef, double %in2, i32 2
  %invec1.1 = insertelement <2 x double> %invec1.0, double %in3, i32 3
  %retvec1 = tail call <2 x double> @__vrd2_cos(<2 x double> %invec1.1) nounwind readnone
  %ret0 = extractelement <2 x double> %retvec0, i32 0
  %ret1 = extractelement <2 x double> %retvec0, i32 1
  %ret2 = extractelement <2 x double> %retvec1, i32 0
  %ret3 = extractelement <2 x double> %retvec1, i32 1
  %retvec2.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec2.1 = insertelement <4 x double> %retvec2.0, double %ret1, i32 1
  %retvec2.2 = insertelement <4 x double> %retvec2.1, double %ret2, i32 2
  %retvec2.3 = insertelement <4 x double> %retvec2.2, double %ret3, i32 3
  ret <4 x double> %retvec2.3
}

define <4 x double> @vecmath_log2_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %in1 = extractelement <4 x double> %ins, i32 1
  %invec0.0 = insertelement <2 x double> undef, double %in0, i32 0
  %invec0.1 = insertelement <2 x double> %invec0.0, double %in1, i32 1
  %retvec0 = tail call <2 x double> @__vrd2_log2(<2 x double> %invec0.1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 0
  %in3 = extractelement <4 x double> %ins, i32 1
  %invec1.0 = insertelement <2 x double> undef, double %in2, i32 2
  %invec1.1 = insertelement <2 x double> %invec1.0, double %in3, i32 3
  %retvec1 = tail call <2 x double> @__vrd2_log2(<2 x double> %invec1.1) nounwind readnone
  %ret0 = extractelement <2 x double> %retvec0, i32 0
  %ret1 = extractelement <2 x double> %retvec0, i32 1
  %ret2 = extractelement <2 x double> %retvec1, i32 0
  %ret3 = extractelement <2 x double> %retvec1, i32 1
  %retvec2.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec2.1 = insertelement <4 x double> %retvec2.0, double %ret1, i32 1
  %retvec2.2 = insertelement <4 x double> %retvec2.1, double %ret2, i32 2
  %retvec2.3 = insertelement <4 x double> %retvec2.2, double %ret3, i32 3
  ret <4 x double> %retvec2.3
}

define <4 x double> @vecmath_log1p_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %in1 = extractelement <4 x double> %ins, i32 1
  %invec0.0 = insertelement <2 x double> undef, double %in0, i32 0
  %invec0.1 = insertelement <2 x double> %invec0.0, double %in1, i32 1
  %retvec0 = tail call <2 x double> @__vrd2_log1p(<2 x double> %invec0.1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 0
  %in3 = extractelement <4 x double> %ins, i32 1
  %invec1.0 = insertelement <2 x double> undef, double %in2, i32 2
  %invec1.1 = insertelement <2 x double> %invec1.0, double %in3, i32 3
  %retvec1 = tail call <2 x double> @__vrd2_log1p(<2 x double> %invec1.1) nounwind readnone
  %ret0 = extractelement <2 x double> %retvec0, i32 0
  %ret1 = extractelement <2 x double> %retvec0, i32 1
  %ret2 = extractelement <2 x double> %retvec1, i32 0
  %ret3 = extractelement <2 x double> %retvec1, i32 1
  %retvec2.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec2.1 = insertelement <4 x double> %retvec2.0, double %ret1, i32 1
  %retvec2.2 = insertelement <4 x double> %retvec2.1, double %ret2, i32 2
  %retvec2.3 = insertelement <4 x double> %retvec2.2, double %ret3, i32 3
  ret <4 x double> %retvec2.3
}

define <4 x double> @vecmath_cbrt_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %in1 = extractelement <4 x double> %ins, i32 1
  %invec0.0 = insertelement <2 x double> undef, double %in0, i32 0
  %invec0.1 = insertelement <2 x double> %invec0.0, double %in1, i32 1
  %retvec0 = tail call <2 x double> @__vrd2_cbrt(<2 x double> %invec0.1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 0
  %in3 = extractelement <4 x double> %ins, i32 1
  %invec1.0 = insertelement <2 x double> undef, double %in2, i32 2
  %invec1.1 = insertelement <2 x double> %invec1.0, double %in3, i32 3
  %retvec1 = tail call <2 x double> @__vrd2_cbrt(<2 x double> %invec1.1) nounwind readnone
  %ret0 = extractelement <2 x double> %retvec0, i32 0
  %ret1 = extractelement <2 x double> %retvec0, i32 1
  %ret2 = extractelement <2 x double> %retvec1, i32 0
  %ret3 = extractelement <2 x double> %retvec1, i32 1
  %retvec2.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec2.1 = insertelement <4 x double> %retvec2.0, double %ret1, i32 1
  %retvec2.2 = insertelement <4 x double> %retvec2.1, double %ret2, i32 2
  %retvec2.3 = insertelement <4 x double> %retvec2.2, double %ret3, i32 3
  ret <4 x double> %retvec2.3
}

define <4 x double> @vecmath_expm1_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %in1 = extractelement <4 x double> %ins, i32 1
  %invec0.0 = insertelement <2 x double> undef, double %in0, i32 0
  %invec0.1 = insertelement <2 x double> %invec0.0, double %in1, i32 1
  %retvec0 = tail call <2 x double> @__vrd2_expm1(<2 x double> %invec0.1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 0
  %in3 = extractelement <4 x double> %ins, i32 1
  %invec1.0 = insertelement <2 x double> undef, double %in2, i32 2
  %invec1.1 = insertelement <2 x double> %invec1.0, double %in3, i32 3
  %retvec1 = tail call <2 x double> @__vrd2_expm1(<2 x double> %invec1.1) nounwind readnone
  %ret0 = extractelement <2 x double> %retvec0, i32 0
  %ret1 = extractelement <2 x double> %retvec0, i32 1
  %ret2 = extractelement <2 x double> %retvec1, i32 0
  %ret3 = extractelement <2 x double> %retvec1, i32 1
  %retvec2.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec2.1 = insertelement <4 x double> %retvec2.0, double %ret1, i32 1
  %retvec2.2 = insertelement <4 x double> %retvec2.1, double %ret2, i32 2
  %retvec2.3 = insertelement <4 x double> %retvec2.2, double %ret3, i32 3
  ret <4 x double> %retvec2.3
}

define <4 x double> @vecmath_cosh_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %in1 = extractelement <4 x double> %ins, i32 1
  %invec0.0 = insertelement <2 x double> undef, double %in0, i32 0
  %invec0.1 = insertelement <2 x double> %invec0.0, double %in1, i32 1
  %retvec0 = tail call <2 x double> @__vrd2_cosh(<2 x double> %invec0.1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 0
  %in3 = extractelement <4 x double> %ins, i32 1
  %invec1.0 = insertelement <2 x double> undef, double %in2, i32 2
  %invec1.1 = insertelement <2 x double> %invec1.0, double %in3, i32 3
  %retvec1 = tail call <2 x double> @__vrd2_cosh(<2 x double> %invec1.1) nounwind readnone
  %ret0 = extractelement <2 x double> %retvec0, i32 0
  %ret1 = extractelement <2 x double> %retvec0, i32 1
  %ret2 = extractelement <2 x double> %retvec1, i32 0
  %ret3 = extractelement <2 x double> %retvec1, i32 1
  %retvec2.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec2.1 = insertelement <4 x double> %retvec2.0, double %ret1, i32 1
  %retvec2.2 = insertelement <4 x double> %retvec2.1, double %ret2, i32 2
  %retvec2.3 = insertelement <4 x double> %retvec2.2, double %ret3, i32 3
  ret <4 x double> %retvec2.3
}

define <4 x double> @vecmath_exp_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %in1 = extractelement <4 x double> %ins, i32 1
  %invec0.0 = insertelement <2 x double> undef, double %in0, i32 0
  %invec0.1 = insertelement <2 x double> %invec0.0, double %in1, i32 1
  %retvec0 = tail call <2 x double> @__vrd2_exp(<2 x double> %invec0.1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 0
  %in3 = extractelement <4 x double> %ins, i32 1
  %invec1.0 = insertelement <2 x double> undef, double %in2, i32 2
  %invec1.1 = insertelement <2 x double> %invec1.0, double %in3, i32 3
  %retvec1 = tail call <2 x double> @__vrd2_exp(<2 x double> %invec1.1) nounwind readnone
  %ret0 = extractelement <2 x double> %retvec0, i32 0
  %ret1 = extractelement <2 x double> %retvec0, i32 1
  %ret2 = extractelement <2 x double> %retvec1, i32 0
  %ret3 = extractelement <2 x double> %retvec1, i32 1
  %retvec2.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec2.1 = insertelement <4 x double> %retvec2.0, double %ret1, i32 1
  %retvec2.2 = insertelement <4 x double> %retvec2.1, double %ret2, i32 2
  %retvec2.3 = insertelement <4 x double> %retvec2.2, double %ret3, i32 3
  ret <4 x double> %retvec2.3
}

define <4 x double> @vecmath_exp2_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %in1 = extractelement <4 x double> %ins, i32 1
  %invec0.0 = insertelement <2 x double> undef, double %in0, i32 0
  %invec0.1 = insertelement <2 x double> %invec0.0, double %in1, i32 1
  %retvec0 = tail call <2 x double> @__vrd2_exp2(<2 x double> %invec0.1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 0
  %in3 = extractelement <4 x double> %ins, i32 1
  %invec1.0 = insertelement <2 x double> undef, double %in2, i32 2
  %invec1.1 = insertelement <2 x double> %invec1.0, double %in3, i32 3
  %retvec1 = tail call <2 x double> @__vrd2_exp2(<2 x double> %invec1.1) nounwind readnone
  %ret0 = extractelement <2 x double> %retvec0, i32 0
  %ret1 = extractelement <2 x double> %retvec0, i32 1
  %ret2 = extractelement <2 x double> %retvec1, i32 0
  %ret3 = extractelement <2 x double> %retvec1, i32 1
  %retvec2.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec2.1 = insertelement <4 x double> %retvec2.0, double %ret1, i32 1
  %retvec2.2 = insertelement <4 x double> %retvec2.1, double %ret2, i32 2
  %retvec2.3 = insertelement <4 x double> %retvec2.2, double %ret3, i32 3
  ret <4 x double> %retvec2.3
}

define <4 x double> @vecmath_exp10_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %in1 = extractelement <4 x double> %ins, i32 1
  %invec0.0 = insertelement <2 x double> undef, double %in0, i32 0
  %invec0.1 = insertelement <2 x double> %invec0.0, double %in1, i32 1
  %retvec0 = tail call <2 x double> @__vrd2_exp10(<2 x double> %invec0.1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 0
  %in3 = extractelement <4 x double> %ins, i32 1
  %invec1.0 = insertelement <2 x double> undef, double %in2, i32 2
  %invec1.1 = insertelement <2 x double> %invec1.0, double %in3, i32 3
  %retvec1 = tail call <2 x double> @__vrd2_exp10(<2 x double> %invec1.1) nounwind readnone
  %ret0 = extractelement <2 x double> %retvec0, i32 0
  %ret1 = extractelement <2 x double> %retvec0, i32 1
  %ret2 = extractelement <2 x double> %retvec1, i32 0
  %ret3 = extractelement <2 x double> %retvec1, i32 1
  %retvec2.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec2.1 = insertelement <4 x double> %retvec2.0, double %ret1, i32 1
  %retvec2.2 = insertelement <4 x double> %retvec2.1, double %ret2, i32 2
  %retvec2.3 = insertelement <4 x double> %retvec2.2, double %ret3, i32 3
  ret <4 x double> %retvec2.3
}

define <4 x double> @vecmath_log10_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %in1 = extractelement <4 x double> %ins, i32 1
  %invec0.0 = insertelement <2 x double> undef, double %in0, i32 0
  %invec0.1 = insertelement <2 x double> %invec0.0, double %in1, i32 1
  %retvec0 = tail call <2 x double> @__vrd2_log10(<2 x double> %invec0.1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 0
  %in3 = extractelement <4 x double> %ins, i32 1
  %invec1.0 = insertelement <2 x double> undef, double %in2, i32 2
  %invec1.1 = insertelement <2 x double> %invec1.0, double %in3, i32 3
  %retvec1 = tail call <2 x double> @__vrd2_log10(<2 x double> %invec1.1) nounwind readnone
  %ret0 = extractelement <2 x double> %retvec0, i32 0
  %ret1 = extractelement <2 x double> %retvec0, i32 1
  %ret2 = extractelement <2 x double> %retvec1, i32 0
  %ret3 = extractelement <2 x double> %retvec1, i32 1
  %retvec2.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec2.1 = insertelement <4 x double> %retvec2.0, double %ret1, i32 1
  %retvec2.2 = insertelement <4 x double> %retvec2.1, double %ret2, i32 2
  %retvec2.3 = insertelement <4 x double> %retvec2.2, double %ret3, i32 3
  ret <4 x double> %retvec2.3
}

define <4 x double> @vecmath_sin_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %in1 = extractelement <4 x double> %ins, i32 1
  %invec0.0 = insertelement <2 x double> undef, double %in0, i32 0
  %invec0.1 = insertelement <2 x double> %invec0.0, double %in1, i32 1
  %retvec0 = tail call <2 x double> @__vrd2_sin(<2 x double> %invec0.1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 0
  %in3 = extractelement <4 x double> %ins, i32 1
  %invec1.0 = insertelement <2 x double> undef, double %in2, i32 2
  %invec1.1 = insertelement <2 x double> %invec1.0, double %in3, i32 3
  %retvec1 = tail call <2 x double> @__vrd2_sin(<2 x double> %invec1.1) nounwind readnone
  %ret0 = extractelement <2 x double> %retvec0, i32 0
  %ret1 = extractelement <2 x double> %retvec0, i32 1
  %ret2 = extractelement <2 x double> %retvec1, i32 0
  %ret3 = extractelement <2 x double> %retvec1, i32 1
  %retvec2.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec2.1 = insertelement <4 x double> %retvec2.0, double %ret1, i32 1
  %retvec2.2 = insertelement <4 x double> %retvec2.1, double %ret2, i32 2
  %retvec2.3 = insertelement <4 x double> %retvec2.2, double %ret3, i32 3
  ret <4 x double> %retvec2.3
}

define <4 x double> @vecmath_log_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %in1 = extractelement <4 x double> %ins, i32 1
  %invec0.0 = insertelement <2 x double> undef, double %in0, i32 0
  %invec0.1 = insertelement <2 x double> %invec0.0, double %in1, i32 1
  %retvec0 = tail call <2 x double> @__vrd2_log(<2 x double> %invec0.1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 0
  %in3 = extractelement <4 x double> %ins, i32 1
  %invec1.0 = insertelement <2 x double> undef, double %in2, i32 2
  %invec1.1 = insertelement <2 x double> %invec1.0, double %in3, i32 3
  %retvec1 = tail call <2 x double> @__vrd2_log(<2 x double> %invec1.1) nounwind readnone
  %ret0 = extractelement <2 x double> %retvec0, i32 0
  %ret1 = extractelement <2 x double> %retvec0, i32 1
  %ret2 = extractelement <2 x double> %retvec1, i32 0
  %ret3 = extractelement <2 x double> %retvec1, i32 1
  %retvec2.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec2.1 = insertelement <4 x double> %retvec2.0, double %ret1, i32 1
  %retvec2.2 = insertelement <4 x double> %retvec2.1, double %ret2, i32 2
  %retvec2.3 = insertelement <4 x double> %retvec2.2, double %ret3, i32 3
  ret <4 x double> %retvec2.3
}

define <4 x double> @vecmath_asin_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @asin(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @asin(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @asin(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @asin(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_asinh_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @asinh(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @asinh(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @asinh(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @asinh(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_atan_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @atan(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @atan(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @atan(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @atan(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_tanh_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @tanh(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @tanh(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @tanh(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @tanh(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_atanh_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @atanh(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @atanh(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @atanh(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @atanh(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_sqrt_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @sqrt(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @sqrt(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @sqrt(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @sqrt(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_erf_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @erf(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @erf(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @erf(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @erf(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_logb_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @logb(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @logb(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @logb(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @logb(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_sinh_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @sinh(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @sinh(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @sinh(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @sinh(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_acosh_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @acosh(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @acosh(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @acosh(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @acosh(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_pow10_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @pow10(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @pow10(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @pow10(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @pow10(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_erfc_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @erfc(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @erfc(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @erfc(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @erfc(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_tan_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @tan(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @tan(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @tan(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @tan(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_acos_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @acos(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @acos(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @acos(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @acos(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

;;[[[end]]]

