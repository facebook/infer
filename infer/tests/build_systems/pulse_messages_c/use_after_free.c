/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void uaf_intraprocedural_bad(int* x) {
  free(x);
  int _ = *x;
}

void free_wrapper(int* p) { free(p); }

void free_wrapper_uaf_bad(int* x) {
  free_wrapper(x);
  int _ = *x;
}

void deref_int(int* x) { int _ = *x; }

void uaf_via_deref_bad(int* x) {
  free(x);
  deref_int(x);
}

void uaf_via_deref_and_free_wrapper_bad(int* x) {
  free_wrapper(x);
  deref_int(x);
}
