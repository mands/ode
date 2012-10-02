;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; VecMath Wrapper onto AMD LibM library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; scalars - 1xf64 (libm SSE2/x87)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; thunking functions
define double @vecmath_exp_f64(double %in) nounwind alwaysinline readnone {
entry:
  %call = tail call double @amd_exp(double %in) nounwind readnone
  ret double %call
}

;;; declarations
declare double @amd_exp(double) nounwind readnone


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vectors - 2xf64 (SSE2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; thunking functions
define <2 x double> @vecmath_exp_v2f64(<2 x double> %ins) nounwind alwaysinline readnone {
entry:
  %call = tail call <2 x double> @amd_vrd2_exp(<2 x double> %ins) nounwind readnone
  ret <2 x double> %call
}

;;; declarations
declare <2 x double> @amd_vrd2_exp(<2 x double>) nounwind readnone


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; vectors - 4xf64 (AVX)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

