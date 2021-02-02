/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int main() {
  int x = 0;
  while (__llair_choice()) {
    x += 1;
    while (__llair_choice()) {
      x += 3;
      if (__llair_choice()) {
        x += 5;
      } else {
        x += 7;
      }
      if (__llair_choice())
        break;
    }
  }
  return x;
}
