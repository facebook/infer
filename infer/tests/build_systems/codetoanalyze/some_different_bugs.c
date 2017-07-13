/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdlib.h>

void test1() {
  int* s1 = NULL;
  *s1 = 42;
}

void test3() {
  int* s3 = malloc(1);
  if (s3 != NULL) {
    *s3 = 42;
  }
}
