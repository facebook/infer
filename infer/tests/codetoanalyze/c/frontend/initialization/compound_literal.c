/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

struct point {
  int x;
  int y;
};

int compound_literal_expr() { return ((struct point){.y = 32, .x = 52}).x; }

int init_with_compound_literal() {
  struct point p = (struct point){32, 52};
  return 1 / (p.x - 32);
}
