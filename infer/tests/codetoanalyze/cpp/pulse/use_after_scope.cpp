/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stddef.h>

void invalidate_local_ok(int** pp) {
  int t = 0xdeadbeef;
  *pp = &t; // <-- potential bug here since t goes out of scope
}

void access_out_of_scope_stack_ref_bad() {
  int* p = NULL;
  invalidate_local_ok(&p);
  int k = *p; // accessing invalid
}

void no_access_out_of_scope_stack_ref_ok() {
  int* p = NULL;
  invalidate_local_ok(&p);
  // p is not accessed, hence ok
}
