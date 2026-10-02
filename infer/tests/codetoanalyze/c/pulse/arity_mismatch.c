/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

// These functions are defined in arity_mismatch2.c with different parameters,
// as if by another program analyzed together with this file. C functions are
// identified by their name only, so the calls below resolve to those
// definitions.
int defined_elsewhere_without_params(int fd, int how);
int* defined_elsewhere_without_params_returns_null(int* p);
void defined_elsewhere_with_two_params(int* p);

int more_actuals_than_formals_then_npe_bad(int fd) {
  int* p = NULL;
  defined_elsewhere_without_params(fd, 2);
  return *p;
}

int fewer_actuals_than_formals_then_npe_bad() {
  int x = 0;
  int* p = NULL;
  defined_elsewhere_with_two_params(&x);
  return *p;
}

void arity_mismatch_then_leak_bad() {
  int* p = (int*)malloc(sizeof(int));
  defined_elsewhere_without_params(0, 2);
}

void arity_mismatch_havocs_args_ok() {
  int* p = (int*)malloc(sizeof(int));
  defined_elsewhere_without_params_returns_null(p);
}

void arity_mismatch_ignores_definition_ok() {
  int x = 0;
  int* p = defined_elsewhere_without_params_returns_null(&x);
  *p = 42;
}

int* returns_null_elsewhere(void);

void arity_match_uses_definition_bad() {
  int* p = returns_null_elsewhere();
  *p = 42;
}

void deref_param(int* x) { *x = 42; }

void cast_function_pointer_arity_mismatch_then_npe_bad() {
  int* p = NULL;
  ((void (*)(int*, int))deref_param)(p, 0);
  *p = 42;
}

void function_pointer_variable_arity_mismatch_then_npe_bad() {
  void (*f)(int*, int) = (void (*)(int*, int))deref_param;
  int* p = NULL;
  f(p, 0);
  *p = 42;
}

int* global_set_by_callee;

void set_global(int* v) { global_set_by_callee = v; }

// the definition is not applied and unknown calls do not havoc globals
int FP_cast_function_pointer_arity_mismatch_sets_global_ok(int* v) {
  global_set_by_callee = NULL;
  ((void (*)(int*, int))set_global)(v, 1);
  return *global_set_by_callee;
}

int* kr_returns_null(p, q)
int* p;
int* q;
{
  return NULL;
}

int kr_definition_fewer_actuals_then_npe_bad() {
  int x = 0;
  int* p = NULL;
  kr_returns_null(&x);
  return *p;
}

int kr_definition_matching_arity_uses_definition_bad() {
  int x = 0;
  int* p = kr_returns_null(&x, &x);
  return *p;
}

int* variadic_returns_null(int n, ...) { return NULL; }

int variadic_extra_actuals_use_definition_bad() {
  int* p = variadic_returns_null(1, 2, 3);
  return *p;
}

__attribute__((noreturn)) void exit_with_failure(void) { exit(1); }

// the noreturn attribute of the definition still applies
void cast_noreturn_function_arity_mismatch_ok() {
  int* p = NULL;
  ((void (*)(int))exit_with_failure)(1);
  *p = 42;
}

// like err() from <err.h>, but arity_mismatch2.c defines a function with this
// name that returns
__attribute__((noreturn)) void fails_here_returns_elsewhere(int status,
                                                            const char* msg);

// only the attributes of the definition are known, so the call returns
void FP_noreturn_declaration_arity_mismatch_ok() {
  int* p = (int*)malloc(sizeof(int));
  if (p == NULL) {
    fails_here_returns_elsewhere(1, "out of memory");
  }
  *p = 42;
  free(p);
}
