/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
void array_access_Ok(int x, int y) {
  int a[1];
  if (x + y == 1) {
    a[x + y - 1] = 0;
  }
}

void call_array_access_Ok() { array_access_Ok(0, 0); }

void FP_array_access2_Ok(int x, int y) {
  int a[1];
  if (x + y == 1) {
    if (x + y != 1) {
      a[3] = 0;
    }
  }
}

void FP_array_access3_Ok(int x, int* y) {
  int a[1];
  *y = x + 1;
  if (x + 1 == 1) {
    if (*y != 1) {
      a[3] = 0;
    }
  }
}

void FP_array_access4_Ok(int x, int* y) {
  int a[1];
  *y = x * 3 + 1;
  if (x * 3 + 1 == 1) {
    if (*y != 1) {
      a[3] = 0;
    }
  }
}
