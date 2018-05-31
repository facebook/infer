/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

void prune_constant_not_Bad() {
  int x = 0;
  int a[1];
  if (!x) {
    a[x + 1] = 0;
  }
}
