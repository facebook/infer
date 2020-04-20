/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

int* malloc_no_check_bad() {
  int* p = malloc(sizeof(int));
  *p = 42;
  return p;
}

void create_null_path_ok(int* p) {
  if (p) {
    *p = 32;
  }
}

void call_create_null_path_then_deref_unconditionally_ok(int* p) {
  create_null_path_ok(p);
  *p = 52;
}

void create_null_path2_ok(int* p) {
  int* q = NULL;
  if (p) {
    *p = 32;
  }
  // arguably bogus to check p above but not here, but the above could
  // also be macro-generated code so both reporting and not reporting
  // are sort of justifiable
  *p = 52;
}

// combine several of the difficulties above
void malloc_then_call_create_null_path_then_deref_unconditionally_ok(int* p) {
  int* x = malloc(sizeof(int));
  if (p) {
    *p = 32;
  }
  create_null_path_ok(p);
  *p = 52;
  free(x);
}
