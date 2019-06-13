/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int main1(int y) {

  static int s = 3;
  int x = 7;

  int add1, add2;
  int (^addblock)(int a, int b);

  addblock = ^(int c, int d) {
    int (^addblock2)(int a);
    int add2;
    int bla = 3;

    addblock2 = ^(int z) {
      return z + s + x + bla;
    };

    add2 = addblock2(1);
    return c + add2 + bla;
  };

  add1 = addblock(1, 2);

  addblock = ^(int e, int f) {
    return e - s;
  };

  add2 = addblock(3, 2);

  // Here we should get a division by zero
  y = add1 / add2;

  return y;
}

int BlockMain() { return main1(4); }
