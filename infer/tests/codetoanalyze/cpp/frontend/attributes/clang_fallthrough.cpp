/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
