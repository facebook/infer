/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
void prune_alias_le_Ok(int x) {
  int a[1];

  if (x <= x) {
    a[0] = 0;
  } else {
    a[1] = 0;
  }
}

void prune_alias_ge_Ok(int x) {
  int a[1];

  if (x >= x) {
    a[0] = 0;
  } else {
    a[1] = 0;
  }
}

void prune_alias_eq_Ok(int x) {
  int a[1];

  if (x == x) {
    a[0] = 0;
  } else {
    a[1] = 0;
  }
}

void prune_alias_lt_Ok(int x) {
  int a[1];

  if (x < x) {
    a[1] = 0;
  }
}

void prune_alias_gt_Ok(int x) {
  int a[1];

  if (x > x) {
    a[1] = 0;
  }
}

void prune_alias_ne_Ok(int x) {
  int a[1];

  if (x != x) {
    a[1] = 0;
  }
}

void prune_alias_not_Ok(int x) {
  int a[1];

  if (!(x == x)) {
    a[1] = 0;
  }

  if (!(x <= x)) {
    a[1] = 0;
  }

  if (!(x >= x)) {
    a[1] = 0;
  }
}

void prune_alias_and_Ok(int x) {
  int a[1];

  if (x == x && x != x) {
    a[1] = 0;
  }
}

void prune_alias_or_Ok(int x, int y) {
  int a[1];

  if (x != x || y != y) {
    a[1] = 0;
  }
}

void prune_alias_exp_Ok(int x) {
  int a[1];

  if (x + 1 != x + 1) {
    a[1] = 0;
  }
}

void FP_prune_alias_exp_Ok(int x) {
  int a[1];

  if (x + 1 != 1 + x) {
    a[1] = 0;
  }
}
