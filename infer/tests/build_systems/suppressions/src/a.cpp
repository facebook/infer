/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void deref_nullptr_bad() {
  int* p = nullptr;
  *p = 42;
}

void deref_nullptr_suppressed() {
  int* p = nullptr;
  // @infer-ignore NULLPTR_DEREFERENCE
  *p = 42;
}

void deref_nullptr2_suppressed() {
  int* p = nullptr;
  *p = 42; // @infer-ignore NULLPTR_DEREFERENCE
}

// @infer-ignore-every USE_AFTER_FREE

int use_after_free_simple_suppressed(int* x) {
  free(x);
  return *x;
}

int use_after_free_simple2_suppressed(int* x) {
  free(x);
  return *x;
}
