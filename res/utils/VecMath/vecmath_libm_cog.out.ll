;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VecMath Wrapper onto GNU libM library (this doesnt apply any vectorisation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; generate prototypes
;;[[[cog 
;;  import cog
;;  import VecMathCog as C
;;  # scalar prototypes
;;  vecSize = 1
;;  for f in C.funcsFtoF:
;;    protoName = C.getCallName(f, vecSize, C.MathLib.GNU)
;;    C.genProto(protoName, vecSize)
;;]]]
declare double @__sin_finite(double %ins) nounwind readnone
declare double @__cos_finite(double %ins) nounwind readnone
declare double @__tan_finite(double %ins) nounwind readnone
declare double @__asin_finite(double %ins) nounwind readnone
declare double @__acos_finite(double %ins) nounwind readnone
declare double @__atan_finite(double %ins) nounwind readnone
declare double @__sinh_finite(double %ins) nounwind readnone
declare double @__cosh_finite(double %ins) nounwind readnone
declare double @__tanh_finite(double %ins) nounwind readnone
declare double @__asinh_finite(double %ins) nounwind readnone
declare double @__acosh_finite(double %ins) nounwind readnone
declare double @__atanh_finite(double %ins) nounwind readnone
declare double @__exp_finite(double %ins) nounwind readnone
declare double @__exp2_finite(double %ins) nounwind readnone
declare double @__exp10_finite(double %ins) nounwind readnone
declare double @__expm1_finite(double %ins) nounwind readnone
declare double @__log_finite(double %ins) nounwind readnone
declare double @__log2_finite(double %ins) nounwind readnone
declare double @__log10_finite(double %ins) nounwind readnone
declare double @__logb_finite(double %ins) nounwind readnone
declare double @__log1p_finite(double %ins) nounwind readnone
declare double @__sqrt_finite(double %ins) nounwind readnone
declare double @__cbrt_finite(double %ins) nounwind readnone
declare double @__pow10_finite(double %ins) nounwind readnone
declare double @__erf_finite(double %ins) nounwind readnone
declare double @__erfc_finite(double %ins) nounwind readnone
;;[[[end]]]


;;[[[cog 
;;  # scalar thunks
;;  vecSize = 1
;;  for f in C.funcsFtoF:
;;    callName = C.getCallName(f, vecSize, C.MathLib.GNU)
;;    C.genThunk(f, callName, vecSize, 1)
;;]]]
define double @vecmath_sin_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__sin_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_cos_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__cos_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_tan_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__tan_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_asin_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__asin_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_acos_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__acos_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_atan_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__atan_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_sinh_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__sinh_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_cosh_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__cosh_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_tanh_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__tanh_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_asinh_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__asinh_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_acosh_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__acosh_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_atanh_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__atanh_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_exp_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__exp_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_exp2_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__exp2_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_exp10_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__exp10_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_expm1_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__expm1_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_log_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__log_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_log2_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__log2_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_log10_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__log10_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_logb_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__logb_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_log1p_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__log1p_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_sqrt_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__sqrt_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_cbrt_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__cbrt_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_pow10_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__pow10_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_erf_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__erf_finite(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_erfc_f64(double %ins) nounwind alwaysinline readnone {
entry:
  %ret0 = tail call double @__erfc_finite(double %ins) nounwind readnone
  ret double %ret0
}

;;[[[end]]]


;;[[[cog 
;;  # svml thunks
;;  vecSize = 2
;;  for f in C.funcsFtoF:
;;    callName = C.getCallName(f, vecSize, C.MathLib.GNU)
;;    C.genThunk(f, callName, vecSize, 1)
;;]]]
define <2 x double> @vecmath_sin_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__sin_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__sin_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_cos_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__cos_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__cos_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_tan_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__tan_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__tan_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_asin_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__asin_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__asin_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_acos_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__acos_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__acos_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_atan_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__atan_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__atan_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_sinh_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__sinh_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__sinh_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_cosh_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__cosh_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__cosh_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_tanh_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__tanh_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__tanh_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_asinh_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__asinh_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__asinh_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_acosh_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__acosh_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__acosh_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_atanh_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__atanh_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__atanh_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_exp_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__exp_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__exp_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_exp2_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__exp2_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__exp2_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_exp10_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__exp10_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__exp10_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_expm1_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__expm1_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__expm1_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_log_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__log_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__log_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_log2_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__log2_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__log2_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_log10_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__log10_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__log10_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_logb_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__logb_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__logb_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_log1p_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__log1p_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__log1p_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_sqrt_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__sqrt_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__sqrt_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_cbrt_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__cbrt_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__cbrt_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_pow10_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__pow10_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__pow10_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_erf_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__erf_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__erf_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

define <2 x double> @vecmath_erfc_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <2 x double> %ins, i32 0
  %ret0 = tail call double @__erfc_finite(double %in0) nounwind readnone
  %in1 = extractelement <2 x double> %ins, i32 1
  %ret1 = tail call double @__erfc_finite(double %in1) nounwind readnone
  %retvec0.0 = insertelement <2 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <2 x double> %retvec0.0, double %ret1, i32 1
  ret <2 x double> %retvec0.1
}

;;[[[end]]]

;;[[[cog 
;;  # svml thunks
;;  vecSize = 4
;;  for f in C.funcsFtoF:
;;    callName = C.getCallName(f, vecSize, C.MathLib.GNU)
;;    C.genThunk(f, callName, vecSize, 1)
;;]]]
define <4 x double> @vecmath_sin_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__sin_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__sin_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__sin_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__sin_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_cos_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__cos_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__cos_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__cos_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__cos_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_tan_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__tan_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__tan_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__tan_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__tan_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_asin_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__asin_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__asin_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__asin_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__asin_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_acos_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__acos_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__acos_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__acos_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__acos_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_atan_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__atan_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__atan_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__atan_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__atan_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_sinh_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__sinh_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__sinh_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__sinh_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__sinh_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_cosh_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__cosh_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__cosh_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__cosh_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__cosh_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_tanh_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__tanh_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__tanh_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__tanh_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__tanh_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_asinh_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__asinh_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__asinh_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__asinh_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__asinh_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_acosh_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__acosh_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__acosh_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__acosh_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__acosh_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_atanh_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__atanh_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__atanh_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__atanh_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__atanh_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_exp_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__exp_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__exp_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__exp_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__exp_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_exp2_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__exp2_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__exp2_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__exp2_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__exp2_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_exp10_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__exp10_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__exp10_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__exp10_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__exp10_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_expm1_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__expm1_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__expm1_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__expm1_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__expm1_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_log_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__log_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__log_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__log_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__log_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_log2_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__log2_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__log2_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__log2_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__log2_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_log10_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__log10_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__log10_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__log10_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__log10_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_logb_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__logb_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__logb_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__logb_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__logb_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_log1p_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__log1p_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__log1p_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__log1p_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__log1p_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_sqrt_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__sqrt_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__sqrt_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__sqrt_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__sqrt_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_cbrt_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__cbrt_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__cbrt_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__cbrt_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__cbrt_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_pow10_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__pow10_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__pow10_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__pow10_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__pow10_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_erf_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__erf_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__erf_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__erf_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__erf_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

define <4 x double> @vecmath_erfc_v4f64(<4 x double> %ins) nounwind alwaysinline readnone {
entry:
  %in0 = extractelement <4 x double> %ins, i32 0
  %ret0 = tail call double @__erfc_finite(double %in0) nounwind readnone
  %in1 = extractelement <4 x double> %ins, i32 1
  %ret1 = tail call double @__erfc_finite(double %in1) nounwind readnone
  %in2 = extractelement <4 x double> %ins, i32 2
  %ret2 = tail call double @__erfc_finite(double %in2) nounwind readnone
  %in3 = extractelement <4 x double> %ins, i32 3
  %ret3 = tail call double @__erfc_finite(double %in3) nounwind readnone
  %retvec0.0 = insertelement <4 x double> undef, double %ret0, i32 0
  %retvec0.1 = insertelement <4 x double> %retvec0.0, double %ret1, i32 1
  %retvec0.2 = insertelement <4 x double> %retvec0.1, double %ret2, i32 2
  %retvec0.3 = insertelement <4 x double> %retvec0.2, double %ret3, i32 3
  ret <4 x double> %retvec0.3
}

;;[[[end]]]
;;  



