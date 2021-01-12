/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int main() {
  int n = 0;
  int b = __llair_choice();
  for (int i = 0; i < b; i++) {
    n += i;
  }
  return n;
}
