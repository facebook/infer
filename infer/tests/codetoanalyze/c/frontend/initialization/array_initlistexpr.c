/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int init_const_array() {
  int z;
  int a[2][3] = {{z + 1, 2, 3}, {5, 6, 7}};
}

void init_variable_array(int len) {
  int x = 2 * len;
  int a[len + x + 1];
}

void init_array_filler() {
  int a[4] = {1};
  int* b[3] = {};
  int c[4] = {[1] = 1};
  int d[100] = {1, 2};
}
