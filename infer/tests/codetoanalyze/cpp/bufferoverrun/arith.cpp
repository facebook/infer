/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
void sizeof_bool_Good() {
  int a[2];
  int z = sizeof(bool); // z is 1 (byte)
  a[z] = 0;
}

void sizeof_bool_Bad() {
  int a[1];
  int z = sizeof(bool); // z is 1 (byte)
  a[z] = 0;
}

// FP due to incomplete frontend translation of casting
void range_bool_Good_FP() {
  int a[2];
  bool x = true + true; // x is 1 (true)
  a[x] = 0;
}

void range_bool_Bad() {
  int a[1];
  bool x = true + false; // x is 1 (true)
  a[x] = 0;
}

void bool_overflow_Good_FP() {
  int a[10];
  if (((bool)-1) - 1) {
    a[10] = 0;
  } else {
    a[5] = 0;
  }
}

void bool_overflow_Bad() {
  int a[10];
  if (((bool)-1) - 1) {
    a[10] = 0;
  } else {
    a[15] = 0;
  }
}

void bool_overflow2_Good_FP() {
  int a[10];
  if (((bool)-2) - 1) {
    a[10] = 0;
  } else {
    a[5] = 0;
  }
}
