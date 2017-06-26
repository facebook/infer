/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
