/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#define test(x) _Generic((x), _Bool : 1, char : 2, int : 3, default : 4)

void test_typename(void) {
  char s;
  int y;
  int x = test(s);
  int z = test(y);
}
