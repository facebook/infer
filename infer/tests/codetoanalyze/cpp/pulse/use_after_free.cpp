/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

int use_after_free_simple_bad(int* x) {
  free(x);
  return *x;
}

int double_free_simple_bad(int* x) {
  free(x);
  free(x);
}

int* global_pointer;

void free_global_pointer_ok() { free(global_pointer); }

void double_free_global_bad() {
  free_global_pointer_ok();
  free_global_pointer_ok();
}
