/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int test1(bool a, bool b) {
  int x = 0;
  do {
    x = x + 1;
    if (a) {
      x = x + 2;
      continue;
    } else {
      x = x + 3;
    }
    x = x + 4;
  } while (b);
  return x;
}
