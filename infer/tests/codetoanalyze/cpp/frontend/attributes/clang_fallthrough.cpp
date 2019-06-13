/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int h() { return 3; }

int switch_with_fallthrough(int n) {
  int res = 5;
  switch (n) {
    case 22:
    case 33:
      [[clang::fallthrough]];
    case 66:
      [[clang::fallthrough]];
    case 77:
      res = h();
      break;
  }
  return res;
}

int test_fallthrough() { return 1 / (switch_with_fallthrough(66) - 3); }
