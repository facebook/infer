/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
