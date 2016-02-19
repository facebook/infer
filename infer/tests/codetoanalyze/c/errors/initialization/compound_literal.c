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

int return_zero() { return ((struct point){.y = 32, .x = 0}).x; }

int divide_by_zero() { return 1 / return_zero(); }
