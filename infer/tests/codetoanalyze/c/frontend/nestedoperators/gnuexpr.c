/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int main() {
  int y = 3;

  y = ({
    int X = 4;
    X;
  });
  return 0;
}

int test(int* p) {
  return ({
    int x = *p;
    int y = 1;
    x + y;
  });
}

int with_conditional(int* p) {
  return ({
    int x = 1;
    p ? *p + x : x;
  });
}
