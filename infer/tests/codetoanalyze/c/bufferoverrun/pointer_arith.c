/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

void FN_pointer_arith_bad() {
  char arr[10];
  int x = 0;
  if (&x - 1 == 0)
    arr[10] = 1;
}

void array_pointer_arith_Bad() {
  int arr[10];
  int* p = &arr[5];
  p[5] = 1;
}
