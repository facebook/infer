/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

void if_freed_invalid_latent(int x, int* y) {
  if (x > 5) {
    free(y);
    *y = 1;
  }
}

void FN_call_if_freed_invalid_latent(int x) {
  if (x > 0) {
    if_freed_invalid(x, NULL);
  }
}

void FN_call_if_freed_invalid2_bad() { call_if_freed_invalid(7); }
