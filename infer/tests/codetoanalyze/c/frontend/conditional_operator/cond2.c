/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int foo() {
  int x = 5;
  if (3 < 4 || 7 < (x++)) {
    x = 0;
  };
  int y = 19;
  int n = ((3 < 4 || 7 < ((x++) - y)) ? 1 : 2);
  n = (2 < 1 ? 1 : (5 > 4 ? 1 : 2));
  return (0 + (7 > 9 ? 1 : 0));
}

int bar() {
  int x, y;
  y = (x = 1) > 1 ? (++x) : (x--);
  return (0 + ((3 > 4 ? 1 : 2) > 1 ? (x = 1) : 0));
}
