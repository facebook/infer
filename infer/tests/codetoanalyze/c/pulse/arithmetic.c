/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>
#include <stdlib.h>

int return_non_negative() {
  int x = random();
  if (x < 0) {
    exit(1);
  }
  return x;
}

void return_non_negative_is_non_negative_ok() {
  if (return_non_negative() < 0) {
    int* p = NULL;
    *p = 42;
  }
}

void assume_non_negative(int x) {
  if (x < 0) {
    exit(1);
  }
}

void assume_non_negative_is_non_negative_ok() {
  int x = random();
  assume_non_negative(x);
  if (x < 0) {
    int* p = NULL;
    *p = 42;
  }
}
