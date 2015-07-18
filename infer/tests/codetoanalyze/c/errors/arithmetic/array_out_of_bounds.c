/*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
 */

void bound_error() {
  int a[7];
  a[7] = 4;
}

void nested_array_ok() {
  int a[3][4][5];
  a[2][3][4] = 0;
}

void bound_error_nested() {
  int a[3][4][5];
  a[4][3][2] = 0;
}
