/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
