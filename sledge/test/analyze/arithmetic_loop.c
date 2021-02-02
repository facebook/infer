/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int f(int x);
int g(int x);

int f(int x) { // x= 0, 1, 2, ...
  return g(x + 2);
}

int g(int y) { // y= 2, 3, 4, ...
  if (y > 5) {
    return y;
  }
  return f(y - 1); // Back edge!
}

int main() {
  f(0);
  return 0;
}
