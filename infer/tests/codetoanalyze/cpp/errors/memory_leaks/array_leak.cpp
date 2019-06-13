/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int no_leak() {
  int* arr = new int[5];
  arr[0] = 1;
  arr[1] = 2;
  int res = arr[0] + arr[1];
  delete[] arr;
  return res;
}

int leak() {
  int* arr = new int[5];
  arr[0] = 1;
  arr[1] = 2;
  int res = arr[0] + arr[1];
  return res;
}
