/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int sum(int n) {
  int s = 0;
  int i = 0;
  while (i < n) {
    s = s + i;
    i = i + 1;
  }
  return s;
}
