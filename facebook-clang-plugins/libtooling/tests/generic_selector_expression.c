/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#define test(x) _Generic((x), _Bool : 1, char : 2, int : 3, default : 4)

int test_typename(void) {
  char s;
  int y;
  int x = test(s);
  int z = test(y);
  return x + z;
};
