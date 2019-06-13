/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int main() {
  int x = 1;
  int y;

  y = ~x;
  y = -x;
  y = +x;

  y = x++;
  y = ++x;

  y = --x;
  y = x--;

  int a;
  int* b;

  b = &a;
  a = *(b + 1);
  *b = *b + 1;
  a = *(&a);

  return 0;
}
