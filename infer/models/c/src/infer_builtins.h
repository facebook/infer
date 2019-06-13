/*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// builtins to be used to model library functions

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#ifdef __cplusplus
// specify C linkage to avoid mangling of names of infer builtins
// they need to have plain C name so backend can recognise it
extern "C" {
#endif

// model returning an arbitrary (nondeterministic) short
short __infer_nondet_short();

// model returning an arbitrary (nondeterministic) int
int __infer_nondet_int();

// model returning an arbitrary (nondeterministic) long int
long int __infer_nondet_long_int();

// model returning an arbitrary (nondeterministic) long long int
long long int __infer_nondet_long_long_int();

// model returning an arbitrary (nondeterministic) unsigned long int
unsigned long int __infer_nondet_unsigned_long_int();

// model returning an arbitrary (nondeterministic) pointer
void* __infer_nondet_ptr();

// model returning an arbitrary (nondeterministic) float
float __infer_nondet_float();

// model returning an arbitrary (nondeterministic) double
double __infer_nondet_double();

// model returning an arbitrary (nondeterministic) long double
long double __infer_nondet_long_double();

// model returning an arbitrary (nondeterministic) size_t
size_t __infer_nondet_size_t();

// model returning an arbitrary (nondeterministic) time_t
time_t __infer_nondet_time_t();

// model returning an arbitrary (nondeterministic) clock_t
clock_t __infer_nondet_clock_t();

// assume that the cond is false
// and add any constraints to the precondition so that cond is false, if
// possible
#define INFER_EXCLUDE_CONDITION(cond) \
  if (cond)                           \
    while (1)

#ifdef NO_INFER_FAIL
#define INFER_EXCLUDE_CONDITION_MSG(cond, msg) INFER_EXCLUDE_CONDITION(cond)
#else
void __infer_fail(char*);
#define INFER_EXCLUDE_CONDITION_MSG(cond, msg) \
  if (cond)                                    \
  __infer_fail(msg)
#endif

// builtin: force arr to be an array
extern void __require_allocated_array(const void* arr);

// builtin: return the size of arr
extern size_t __get_array_length(const void* arr);

// builtin: change the attribute of ret to a file attribute
extern void __set_file_attribute(void* ret);

// builtin: change the size of the array to size
extern void __set_array_length(void* ptr, size_t size);

// builtin: set the flag to the given value for the procedure where this call
// appears
extern void __infer_set_flag(char* flag, char* value);

#ifdef __cplusplus
} // extern "C"
#endif
