/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

int double_free_in_catch_bad_FN(int a[10], int b) {
  try {
    if (b > 0) {
      free(a);
      throw(0);
    }
  } catch (...) {
    free(a);
  }
  return 0;
}

void throw_positive(int a[10], int b) {
  if (b > 0) {
    free(a);
    throw(1);
  }
}

int double_free_interproc_bad_FN(int a[10], int b) {
  try {
    throw_positive(a, 2);
  } catch (...) {
    free(a);
  }
  return 0;
}
