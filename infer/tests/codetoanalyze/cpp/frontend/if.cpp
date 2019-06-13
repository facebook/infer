/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void if_then_return(int x) {
  int y = 0;
  if (x == 1234) {
    y = x;
  }
}

void if_then(int x) {
  int y = 0;
  if (x == 1234) {
    y = x;
  }
  y = 111;
}

void if_then_else_return(int x) {
  int y = 0;
  if (x == 1234) {
    y = 111;
  } else {
    y = 222;
  }
}

void if_then_else(int x) {
  int y = 0;
  if (x == 1234) {
    y = 111;
  } else {
    y = 222;
  }
  y = 333;
}

void if_then_cond_var() {
  int y = 0;
  if (int x = 4) {
    y = x;
  }
  y = 111;
}

void if_then_else_cond_var() {
  int y = 0;
  if (int x = 4) {
    y = x;
  } else {
    y = x * 2;
  }
  y = 111;
}

void if_then_init(int x) {
  int y = 0;
  if (int x = 4; x == 4) {
    y = x;
  }
}
