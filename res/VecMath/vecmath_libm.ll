;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VecMath Wrapper onto libm - this will be slower than calling funcs directly
; due to packing/unpacking
; Intel SVML or AMD libM is recommended
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scalars - 1xf64 (libm SSE2/x87)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; thunking functions
define double @vecmath_exp_f64(double %in) nounwind alwaysinline readnone {
entry:
  %call = tail call double @__exp_finite(double %in) nounwind readnone
  ret double %call
}

;;; declarations
declare double @__exp_finite(double) nounwind readnone


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vectors - 2xf64 (SSE2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; thunking functions
define <2 x double> @vecmath_exp_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  ; extract the elems and call the func
  %in1 = extractelement <2 x double> %ins, i32 0
  %call1 = tail call double @__exp_finite(double %in1) nounwind readnone

  ; extract the elems and call the func
  %in2 = extractelement <2 x double> %ins, i32 1
  %call2 = tail call double @__exp_finite(double %in2) nounwind readnone

  ; populate the ret vec
  %retvec.0 = insertelement <2 x double> undef, double %call1, i32 0
  %retvec.1 = insertelement <2 x double> %retvec.0, double %call2, i32 1

  ret <2 x double> %retvec.1
}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vectors - 4xf64 (AVX)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

