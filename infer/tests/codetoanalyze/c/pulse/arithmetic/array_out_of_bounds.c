/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Warray-bounds"
void bound_error() {
  int a[7];
  a[7] = 4;
}
#pragma clang diagnostic pop

void nested_array_ok() {
  int a[3][4][5];
  a[2][3][4] = 0;
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Warray-bounds"
void bound_error_nested() {
  int a[3][4][5];
  a[4][3][2] = 0;
}
#pragma clang diagnostic pop
