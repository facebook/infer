/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
