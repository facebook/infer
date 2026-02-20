/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int add(int x, int y) { return x + y; }

int local_vars(int a) {
  int b = a + 1;
  int c = b * 2;
  return c;
}
