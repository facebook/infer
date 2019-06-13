/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int main() {
  int k = 0;
  for (int i = 0; i < 10; i++) {
    while (k < 10) {
      k++;
    }
  }
  return k;
}
