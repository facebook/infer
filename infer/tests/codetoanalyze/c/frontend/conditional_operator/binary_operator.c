/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
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
