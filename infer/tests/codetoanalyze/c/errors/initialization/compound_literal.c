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

int return_zero() { return ((struct point){.y = 32, .x = 0}).x; }

int divide_by_zero() { return 1 / return_zero(); }
