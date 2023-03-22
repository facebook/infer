/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void multidim_arr1_Good() {
  int a[2][3];
  a[1][0] = 0;
}

void multidim_arr1_Bad() {
  int a[2][3];
  a[2][0] = 0;
}

void multidim_arr2_Good() {
  int a[2][3];
  a[1][2] = 0;
}

void multidim_arr2_Bad() {
  int a[2][3];
  a[1][3] = 0;
}

void multidim_arr3_Good() {
  int a[3][2] = {
      {0, 1},
      {0, 2},
      {0, 3},
  };
}

void multidim_arr4_Good() {
  int a[3][2];
  int* p = a;
  *(p + 5) = 0;
}

void multidim_arr4_Bad() {
  int a[3][2];
  int* p = a;
  *(p + 6) = 0;
}

void multidim_arr5_Good() {
  int a[1][10];
  a[0][0] = 0;
  a[0][5] = 0;
}

void multidim_arr5_Bad() {
  int a[1][10];
  a[0][0] = 0;
  a[0][10] = 0;
}

void multidim_arr6_Good() {
  int a[3][2];
  int b[10];
  int* p = a;
  *p = 5;
  b[a[0][0]] = 1;
}

void multidim_arr6_Bad_FN() {
  int a[3][2];
  int b[5];
  int* p = a;
  *p = 5;
  b[a[0][0]] = 1;
}
