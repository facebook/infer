/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
void prune_constant_true_Ok() {
  int a[1];

  if (1) {
    a[0] = 0;
  } else {
    a[1] = 0;
  }
}

void prune_constant_false_Ok() {
  int a[1];

  if (0) {
    a[1] = 0;
  } else {
    a[0] = 0;
  }
}

void prune_constant_value_Ok(int x) {
  int a[1];
  if (-1 < x && x < 1) {
    if (x) {
      a[1] = 0;
    } else {
      a[0] = 0;
    }
  }
}
