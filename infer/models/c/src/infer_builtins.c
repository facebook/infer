/*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// builtins to be used to model library functions

#include "infer_builtins.h"

// model returning an arbitrary (nondeterministic) short
short __infer_nondet_short() {
  short ret;
  return ret;
}

// model returning an arbitrary (nondeterministic) int
int __infer_nondet_int() {
  int ret;
  return ret;
}

// model returning an arbitrary (nondeterministic) long int
long int __infer_nondet_long_int() {
  long int ret;
  return ret;
}

// model returning an arbitrary (nondeterministic) long long int
long long int __infer_nondet_long_long_int() {
  long long int ret;
  return ret;
}

// model returning an arbitrary (nondeterministic) unsigned long int
unsigned long int __infer_nondet_unsigned_long_int() {
  unsigned long int ret;
  return ret;
}

// model returning an arbitrary (nondeterministic) pointer
void* __infer_nondet_ptr() {
  void* res;
  return res;
}

// model returning an arbitrary (nondeterministic) float
float __infer_nondet_float() {
  float ret;
  return ret;
}

// model returning an arbitrary (nondeterministic) double
double __infer_nondet_double() {
  double ret;
  return ret;
}

// model returning an arbitrary (nondeterministic) long double
long double __infer_nondet_long_double() {
  long double ret;
  return ret;
}

// model returning an arbitrary (nondeterministic) size_t
size_t __infer_nondet_size_t() {
  size_t t;
  return t;
}

// model returning an arbitrary (nondeterministic) time_t
time_t __infer_nondet_time_t() {
  time_t t;
  return t;
}

// model returning an arbitrary (nondeterministic) clock_t
clock_t __infer_nondet_clock_t() {
  clock_t t;
  return t;
}

void* infer__builtin___memset_chk(void* dest,
                                  int val,
                                  unsigned long len,
                                  unsigned long dstlen) {
  INFER_EXCLUDE_CONDITION(dstlen < len);
  return dest;
}
