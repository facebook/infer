/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

float f(int x, float y) { // 8, 3.0
  int z = x * x;          // 64
  return y + z;           // 67.0
}
int g(int y) { // 8
  if (y) {
    return 1;
  } else {
    return 0;
  }
}

int main() {
  float x = 1.5;
  x = x * 2.0; // 3.0
  int y = 8;
  float k = f(y, x); // 67.0
  return g(8);       // 1
}
