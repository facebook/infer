/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
int global;

void compare_global_variable_bad() {
  char arr[10];
  if (global < 10)
    arr[10] = 1;
}

const int global_const_zero = 0;

enum { global_const = global_const_zero };

void compare_global_const_enum_Bad() {
  char arr[10];
  if (global_const < 10)
    arr[10] = 1;
}

void compare_global_const_enum_Good_FP() {
  char arr[10];
  if (global_const > 10)
    arr[10] = 1;
}

const int global_const_ten = 10;

void use_global_const_ten_Good() {
  char arr[20];
  arr[global_const_ten] = 0;
}

void use_global_const_ten_Bad() {
  char arr[5];
  arr[global_const_ten] = 0;
}
