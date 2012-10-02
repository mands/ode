; ModuleID = './vecmath_intel_cog.out.bc'

declare double @sin(double) nounwind readnone

declare double @cos(double) nounwind readnone

declare double @tan(double) nounwind readnone

declare double @asin(double) nounwind readnone

declare double @acos(double) nounwind readnone

declare double @atan(double) nounwind readnone

declare double @sinh(double) nounwind readnone

declare double @cosh(double) nounwind readnone

declare double @tanh(double) nounwind readnone

declare double @asinh(double) nounwind readnone

declare double @acosh(double) nounwind readnone

declare double @atanh(double) nounwind readnone

declare double @exp(double) nounwind readnone

declare double @exp2(double) nounwind readnone

declare double @exp10(double) nounwind readnone

declare double @expm1(double) nounwind readnone

declare double @log(double) nounwind readnone

declare double @log2(double) nounwind readnone

declare double @log10(double) nounwind readnone

declare double @logb(double) nounwind readnone

declare double @log1p(double) nounwind readnone

declare double @sqrt(double) nounwind readnone

declare double @cbrt(double) nounwind readnone

declare double @pow10(double) nounwind readnone

declare double @erf(double) nounwind readnone

declare double @erfc(double) nounwind readnone

declare <2 x double> @__svml_sin2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_cos2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_tan2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_asin2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_acos2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_atan2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_sinh2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_cosh2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_tanh2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_asinh2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_acosh2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_atanh2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_exp2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_exp22(<2 x double>) nounwind readnone

declare <2 x double> @__svml_exp102(<2 x double>) nounwind readnone

declare <2 x double> @__svml_expm12(<2 x double>) nounwind readnone

declare <2 x double> @__svml_log2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_log22(<2 x double>) nounwind readnone

declare <2 x double> @__svml_log102(<2 x double>) nounwind readnone

declare <2 x double> @__svml_logb2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_log1p2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_sqrt2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_cbrt2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_pow102(<2 x double>) nounwind readnone

declare <2 x double> @__svml_erf2(<2 x double>) nounwind readnone

declare <2 x double> @__svml_erfc2(<2 x double>) nounwind readnone

declare <4 x double> @__svml_sin4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_cos4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_tan4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_asin4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_acos4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_atan4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_sinh4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_cosh4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_tanh4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_asinh4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_acosh4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_atanh4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_exp4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_exp24(<4 x double>) nounwind readnone

declare <4 x double> @__svml_exp104(<4 x double>) nounwind readnone

declare <4 x double> @__svml_expm14(<4 x double>) nounwind readnone

declare <4 x double> @__svml_log4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_log24(<4 x double>) nounwind readnone

declare <4 x double> @__svml_log104(<4 x double>) nounwind readnone

declare <4 x double> @__svml_logb4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_log1p4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_sqrt4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_cbrt4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_pow104(<4 x double>) nounwind readnone

declare <4 x double> @__svml_erf4(<4 x double>) nounwind readnone

declare <4 x double> @__svml_erfc4(<4 x double>) nounwind readnone

define double @vecmath_sin_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @sin(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_cos_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @cos(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_tan_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @tan(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_asin_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @asin(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_acos_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @acos(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_atan_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @atan(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_sinh_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @sinh(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_cosh_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @cosh(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_tanh_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @tanh(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_asinh_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @asinh(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_acosh_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @acosh(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_atanh_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @atanh(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_exp_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @exp(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_exp2_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @exp2(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_exp10_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @exp10(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_expm1_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @expm1(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_log_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @log(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_log2_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @log2(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_log10_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @log10(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_logb_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @logb(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_log1p_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @log1p(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_sqrt_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @sqrt(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_cbrt_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @cbrt(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_pow10_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @pow10(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_erf_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @erf(double %ins) nounwind readnone
  ret double %ret0
}

define double @vecmath_erfc_f64(double %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call double @erfc(double %ins) nounwind readnone
  ret double %ret0
}

define <2 x double> @vecmath_sin_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_sin2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_cos_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_cos2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_tan_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_tan2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_asin_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_asin2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_acos_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_acos2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_atan_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_atan2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_sinh_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_sinh2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_cosh_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_cosh2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_tanh_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_tanh2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_asinh_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_asinh2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_acosh_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_acosh2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_atanh_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_atanh2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_exp_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_exp2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_exp2_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_exp22(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_exp10_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_exp102(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_expm1_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_expm12(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_log_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_log2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_log2_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_log22(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_log10_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_log102(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_logb_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_logb2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_log1p_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_log1p2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_sqrt_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_sqrt2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_cbrt_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_cbrt2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_pow10_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_pow102(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_erf_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_erf2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <2 x double> @vecmath_erfc_v2f64(<2 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <2 x double> @__svml_erfc2(<2 x double> %ins) nounwind readnone
  ret <2 x double> %ret0
}

define <4 x double> @vecmath_sin_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_sin4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_cos_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_cos4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_tan_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_tan4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_asin_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_asin4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_acos_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_acos4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_atan_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_atan4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_sinh_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_sinh4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_cosh_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_cosh4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_tanh_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_tanh4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_asinh_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_asinh4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_acosh_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_acosh4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_atanh_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_atanh4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_exp_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_exp4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_exp2_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_exp24(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_exp10_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_exp104(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_expm1_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_expm14(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_log_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_log4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_log2_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_log24(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_log10_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_log104(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_logb_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_logb4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_log1p_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_log1p4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_sqrt_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_sqrt4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_cbrt_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_cbrt4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_pow10_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_pow104(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_erf_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_erf4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}

define <4 x double> @vecmath_erfc_v4f64(<4 x double> %ins) nounwind readnone alwaysinline {
entry:
  %ret0 = tail call <4 x double> @__svml_erfc4(<4 x double> %ins) nounwind readnone
  ret <4 x double> %ret0
}
