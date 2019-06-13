/*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// modelling of math functions

#include "infer_builtins.h"

#include <math.h>

double acos(double x) { return __infer_nondet_double(); }

float acosf(float x) { return __infer_nondet_float(); }

long double acosl(long double x) { return __infer_nondet_long_double(); }

double acosh(double x) { return __infer_nondet_double(); }

float acoshf(float x) { return __infer_nondet_float(); }

long double acoshl(long double x) { return __infer_nondet_long_double(); }

double asin(double x) { return __infer_nondet_double(); }

float asinf(float x) { return __infer_nondet_float(); }

long double asinl(long double x) { return __infer_nondet_long_double(); }

double asinh(double x) { return __infer_nondet_double(); }

float asinhf(float x) { return __infer_nondet_float(); }

long double asinhl(long double x) { return __infer_nondet_long_double(); }

double atan(double x) { return __infer_nondet_double(); }

float atanf(float x) { return __infer_nondet_float(); }

long double atanl(long double x) { return __infer_nondet_long_double(); }

double atanh(double x) { return __infer_nondet_double(); }

float atanhf(float x) { return __infer_nondet_float(); }

long double atanhl(long double x) { return __infer_nondet_long_double(); }

double atan2(double y, double x) { return __infer_nondet_double(); }

float atan2f(float y, float x) { return __infer_nondet_float(); }

long double atan2l(long double y, long double x) {
  return __infer_nondet_long_double();
}

double cbrt(double x) { return __infer_nondet_double(); }

float cbrtf(float x) { return __infer_nondet_float(); }

long double cbrtl(long double x) { return __infer_nondet_long_double(); }

double ceil(double x) { return __infer_nondet_double(); }

float ceilf(float x) { return __infer_nondet_float(); }

long double ceill(long double x) { return __infer_nondet_long_double(); }

double copysign(double x, double y) { return __infer_nondet_double(); }

float copysignf(float x, float y) { return __infer_nondet_float(); }

long double copysignl(long double x, long double y) {
  return __infer_nondet_long_double();
}

double cos(double x) { return __infer_nondet_double(); }

float cosf(float x) { return __infer_nondet_float(); }

long double cosl(long double x) { return __infer_nondet_long_double(); }

double cosh(double x) { return __infer_nondet_double(); }

float coshf(float x) { return __infer_nondet_float(); }

long double coshl(long double x) { return __infer_nondet_long_double(); }

double exp(double x) { return __infer_nondet_double(); }

float expf(float x) { return __infer_nondet_float(); }

long double expl(long double x) { return __infer_nondet_long_double(); }

double exp2(double x) { return __infer_nondet_double(); }

float exp2f(float x) { return __infer_nondet_float(); }

long double exp2l(long double x) { return __infer_nondet_long_double(); }

double expm1(double x) { return __infer_nondet_double(); }

float expm1f(float x) { return __infer_nondet_float(); }

long double expm1l(long double x) { return __infer_nondet_long_double(); }

double erf(double x) { return __infer_nondet_double(); }

float erff(float x) { return __infer_nondet_float(); }

long double erfl(long double x) { return __infer_nondet_long_double(); }

double erfc(double x) { return __infer_nondet_double(); }

float erfcf(float x) { return __infer_nondet_float(); }

long double erfcl(long double x) { return __infer_nondet_long_double(); }

double fabs(double x) { return __infer_nondet_double(); }

float fabsf(float x) { return __infer_nondet_float(); }

long double fabsl(long double x) { return __infer_nondet_long_double(); }

double fdim(double x, double y) { return __infer_nondet_double(); }

float fdimf(float x, float y) { return __infer_nondet_float(); }

long double fdiml(long double x, long double y) {
  return __infer_nondet_long_double();
}

double floor(double x) { return __infer_nondet_double(); }

float floorf(float x) { return __infer_nondet_float(); }

long double floorl(long double x) { return __infer_nondet_long_double(); }

double fma(double x, double y, double z) { return __infer_nondet_double(); }

float fmaf(float x, float y, float z) { return __infer_nondet_float(); }

long double fmal(long double x, long double y, long double z) {
  return __infer_nondet_long_double();
}

double fmax(double x, double y) { return __infer_nondet_double(); }

float fmaxf(float x, float y) { return __infer_nondet_float(); }

long double fmaxl(long double x, long double y) {
  return __infer_nondet_long_double();
}

double fmin(double x, double y) { return __infer_nondet_double(); }

float fminf(float x, float y) { return __infer_nondet_float(); }

long double fminl(long double x, long double y) {
  return __infer_nondet_long_double();
}

double fmod(double x, double y) { return __infer_nondet_double(); }

float fmodf(float x, float y) { return __infer_nondet_float(); }

long double fmodl(long double x, long double y) {
  return __infer_nondet_long_double();
}

double frexp(double value, int* exp) {
  *exp = __infer_nondet_int();
  return __infer_nondet_double();
}

float frexpf(float value, int* exp) {
  *exp = __infer_nondet_int();
  return __infer_nondet_float();
}

long double frexpl(long double value, int* exp) {
  *exp = __infer_nondet_int();
  return __infer_nondet_long_double();
}

double hypot(double x, double y) { return __infer_nondet_double(); }

float hypotf(float x, float y) { return __infer_nondet_float(); }

long double hypotl(long double x, long double y) {
  return __infer_nondet_long_double();
}

int ilogb(double x) { return __infer_nondet_int(); }

int ilogbf(float x) { return __infer_nondet_int(); }

int ilogbl(long double x) { return __infer_nondet_int(); }

double ldexp(double x, int exp) { return __infer_nondet_double(); }

float ldexpf(float x, int exp) { return __infer_nondet_float(); }

long double ldexpl(long double x, int exp) {
  return __infer_nondet_long_double();
}

double lgamma(double x) { return __infer_nondet_double(); }

float lgammaf(float x) { return __infer_nondet_float(); }

long double lgammal(long double x) { return __infer_nondet_long_double(); }

double log(double x) { return __infer_nondet_double(); }

float logf(float x) { return __infer_nondet_float(); }

long double logl(long double x) { return __infer_nondet_long_double(); }

double log10(double x) { return __infer_nondet_double(); }

float log10f(float x) { return __infer_nondet_float(); }

long double log10l(long double x) { return __infer_nondet_long_double(); }

double log1p(double x) { return __infer_nondet_double(); }

float log1pf(float x) { return __infer_nondet_float(); }

long double log1pl(long double x) { return __infer_nondet_long_double(); }

#ifdef log2
#undef log2 // disable expansion of log2
#endif
double log2(double x) { return __infer_nondet_double(); }

#ifdef log2f
#undef log2f // disable expansion of log2f
#endif
float log2f(float x) { return __infer_nondet_float(); }

long double log2l(long double x) { return __infer_nondet_long_double(); }

double logb(double x) { return __infer_nondet_double(); }

float logbf(float x) { return __infer_nondet_float(); }

long double logbl(long double x) { return __infer_nondet_long_double(); }

long int lrint(double x) { return __infer_nondet_long_int(); }

long int lrintf(float x) { return __infer_nondet_long_int(); }

long int lrintl(long double x) { return __infer_nondet_long_int(); }

long long int llrint(double x) { return __infer_nondet_long_long_int(); }

long long int llrintf(float x) { return __infer_nondet_long_long_int(); }

long long int llrintl(long double x) { return __infer_nondet_long_long_int(); }

long int lround(double x) { return __infer_nondet_long_int(); }

long int lroundf(float x) { return __infer_nondet_long_int(); }

long int lroundl(long double x) { return __infer_nondet_long_int(); }

long long int llround(double x) { return __infer_nondet_long_long_int(); }

long long int llroundf(float x) { return __infer_nondet_long_long_int(); }

long long int llroundl(long double x) { return __infer_nondet_long_long_int(); }

double modf(double value, double* iptr) {
  *iptr = __infer_nondet_double();
  return __infer_nondet_double();
}

float modff(float value, float* iptr) {
  *iptr = __infer_nondet_float();
  return __infer_nondet_float();
}

long double modfl(long double value, long double* iptr) {
  *iptr = __infer_nondet_long_double();
  return __infer_nondet_long_double();
}

double nearbyint(double x) { return __infer_nondet_double(); }

double nan(const char* tagp) { return __infer_nondet_double(); }

float nanf(const char* tagp) { return __infer_nondet_float(); }

long double nanl(const char* tagp) { return __infer_nondet_long_double(); }

float nearbyintf(float x) { return __infer_nondet_float(); }

long double nearbyintl(long double x) { return __infer_nondet_long_double(); }

double nextafter(double x, double y) { return __infer_nondet_double(); }

float nextafterf(float x, float y) { return __infer_nondet_float(); }

long double nextafterl(long double x, long double y) {
  return __infer_nondet_long_double();
}

double nexttoward(double x, long double y) { return __infer_nondet_double(); }

float nexttowardf(float x, long double y) { return __infer_nondet_float(); }

long double nexttowardl(long double x, long double y) {
  return __infer_nondet_long_double();
}

double pow(double x, double y) { return __infer_nondet_double(); }

float powf(float x, float y) { return __infer_nondet_float(); }

long double powl(long double x, long double y) {
  return __infer_nondet_long_double();
}

double rint(double x) { return __infer_nondet_double(); }

float rintf(float x) { return __infer_nondet_float(); }

long double rintl(long double x) { return __infer_nondet_long_double(); }

double remainder(double x, double y) { return __infer_nondet_double(); }

float remainderf(float x, float y) { return __infer_nondet_float(); }

long double remainderl(long double x, long double y) {
  return __infer_nondet_long_double();
}

double round(double x) { return __infer_nondet_double(); }

float roundf(float x) { return __infer_nondet_float(); }

long double roundl(long double x) { return __infer_nondet_long_double(); }

double scalbn(double x, int n) { return __infer_nondet_double(); }

float scalbnf(float x, int n) { return __infer_nondet_float(); }

long double scalbnl(long double x, int n) {
  return __infer_nondet_long_double();
}

double scalbln(double x, long int n) { return __infer_nondet_double(); }

float scalblnf(float x, long int n) { return __infer_nondet_float(); }

long double scalblnl(long double x, long int n) {
  return __infer_nondet_long_double();
}

double sin(double x) { return __infer_nondet_double(); }

float sinf(float x) { return __infer_nondet_float(); }

long double sinl(long double x) { return __infer_nondet_long_double(); }

double sinh(double x) { return __infer_nondet_double(); }

float sinhf(float x) { return __infer_nondet_float(); }

long double sinhl(long double x) { return __infer_nondet_long_double(); }

double sqrt(double x) { return __infer_nondet_double(); }

float sqrtf(float x) { return __infer_nondet_float(); }

long double sqrtl(long double x) { return __infer_nondet_long_double(); }

double tan(double x) { return __infer_nondet_double(); }

float tanf(float x) { return __infer_nondet_float(); }

long double tanl(long double x) { return __infer_nondet_long_double(); }

double tanh(double x) { return __infer_nondet_double(); }

float tanhf(float x) { return __infer_nondet_float(); }

long double tanhl(long double x) { return __infer_nondet_long_double(); }

double tgamma(double x) { return __infer_nondet_double(); }

float tgammaf(float x) { return __infer_nondet_float(); }

long double tgammal(long double x) { return __infer_nondet_long_double(); }

double trunc(double x) { return __infer_nondet_double(); }

float truncf(float x) { return __infer_nondet_float(); }

long double truncl(long double x) { return __infer_nondet_long_double(); }
