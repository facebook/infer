/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void FP_local_addr_noalias_ok(int* p) {
  int* q = NULL;
  int x = 1;
  if (&x == p) {
    *q = 42;
  }
}

void local_addr_noalias_bad(int* p) {
  int* q = NULL;
  int x = 1;
  if (&x != p) {
    *q = 42;
  }
}

static int g = 0;
void global_addr_alias_bad(int* p) {
  int* q = NULL;
  if (&g == p) {
    *q = 42;
  }
}

void angelic_treatment_of_funcction_pointers_ok(int* (*fun_pointer)()) {
  int* p = fun_pointer();
  *p = 42;
}

void null_dereference_following_function_pointer_call_bad(
    int* (*fun_pointer)()) {
  int* p = fun_pointer();
  p = NULL;
  *p = 42;
}
