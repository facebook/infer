/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int test1(bool a, bool b) {
  int x = 0;
  do {
    x = x + 1;
    if (a) {
      x = x + 2;
      continue;
    } else {
      x = x + 3;
    }
    x = x + 4;
  } while (b);
  return x;
}
