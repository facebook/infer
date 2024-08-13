/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// pulse doesn't detect array out of bound errors yet

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Warray-bounds"
void FN_const_bound_too_large_bad() {
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
void FN_const_nested_bound_too_large_bad() {
  int a[3][4][5];
  a[4][3][2] = 0;
}
#pragma clang diagnostic pop
