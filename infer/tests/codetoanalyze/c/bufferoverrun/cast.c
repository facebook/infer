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

void cast2_Good_FP() {
  int32_t arr[4];
  *(((char*)arr) + 4) = 123;
}
