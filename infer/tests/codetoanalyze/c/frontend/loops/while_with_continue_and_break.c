/*
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int main() {
  int x = 0;
  while (1) {
    while (2) {
      x += 1;
      if (x > 5) {
        break;
      }
    }
    if (x == 2) {
      continue;
    }
  }
  return 0;
}
