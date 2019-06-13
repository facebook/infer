/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int simple_assignment() {
  int x = 10;
  int result = 0;
  while (int a = x) {
    result += a;
    x -= 1;
  }
  return 0;
}

int conditional_assignment() {
  int x = 10;
  int result = 0;
  while (int a = x > 0 ? x : 0) {
    result += a;
    x -= 1;
  }
  return 0;
}
