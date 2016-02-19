/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

void binop_with_side_effects(int z) {
  // simple assignment
  int x1;
  x1 = (1 ? z : z) + 77;

  int x2;
  x2 = 77 + (1 ? z : z);

  int x3;
  x3 = (1 ? z : z) + (1 ? z : z);

  // initializer
  int y1 = (1 ? z : z) + 77;

  int y2 = 77 + (1 ? z : z);

  int y3 = (1 ? z : z) + (1 ? z : z);
}
