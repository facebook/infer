/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void two_branch() {
  int x;
  if (x) {
    x = 0;
  } else {
    x = 1;
  }
}

void three_branch() {
  int x;
  if (x) {
    if (x) {
      x = 0;
    } else {
      x = 1;
    }
  } else {
    x = 2;
  }
}

void four_branch() {
  int x;
  if (x) {
    if (x) {
      x = 0;
    } else {
      x = 1;
    }
  } else {
    if (x) {
      x = 0;
    } else {
      x = 1;
    }
  }
}

void sequence() {
  two_branch();
  three_branch();
  four_branch();
}

void loop() {
  two_branch();
  for (;;) {
    two_branch();
  }
}
