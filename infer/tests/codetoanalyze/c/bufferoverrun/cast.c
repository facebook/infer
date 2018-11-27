/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdint.h>

void cast_Good() {
  char arr[5];
  *(int32_t*)(arr + 1) = 123;
}

void cast_Bad_FN() {
  char arr[1];
  *(int32_t*)arr = 123;
}

void cast2_Good() {
  int32_t arr[4];
  *(((char*)arr) + 4) = 123;
}

void cast2_Bad() {
  int32_t arr[4];
  *(((char*)arr) + 20) = 123;
}

void cast_unsigned_to_signed_Good() {
  char arr[10];
  uint32_t x = 15;
  int32_t y = (int32_t)x;
  if (y < 10) {
    arr[y] = 0;
  }
}

void cast_unsigned_to_signed_Bad_FN() {
  char arr[10];
  uint32_t x = 4294967295;
  int32_t y = (int32_t)x; // y is -1.
  if (y < 10) {
    arr[y] = 0;
  }
}

void cast_signed_to_unsigned_Good() {
  char arr[10];
  int32_t x = 15;
  uint32_t y = (uint32_t)x;
  if (y < 10) {
    arr[y] = 0;
  }
}

void cast_signed_to_unsigned_Bad() {
  char arr[10];
  int32_t x = -1;
  uint32_t y = (uint32_t)x;
  if (y > 0) {
    arr[y] = 0;
  }
}

void cast_signed_to_unsigned2_Bad_FN() {
  char arr[10];
  int32_t x = -2;
  uint32_t y = (uint32_t)x;
  if (y > 0) {
    arr[y] = 0;
  }
}

void cast_float_to_int_Good() {
  char arr[10];
  float x = 15.0;
  int32_t y = (int32_t)x;
  if (y < 10) {
    arr[y] = 0;
  }
}

void cast_float_to_int_Bad_FN() {
  char arr[10];
  float x = 15000000000.0;
  int32_t y = (int32_t)x; // y is -2147483648.
  if (y < 10) {
    arr[y] = 0;
  }
}
