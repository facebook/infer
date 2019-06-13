/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int main() {
  int j = 0;
  int i = 0;
  for (int b = 3; (b = 10); i++) {
    j += j;
  }
  return 0;
}
