/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int main() {
  int a = 10;
  int b = 0;
  do {
    a = 1;
    do {
      a = 2;
    } while (b < 30);
  } while (b < 20);

  return 0;
}
