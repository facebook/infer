/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* With the dominator approach, we can't find any back-edges here
   since there are two entry points to the loop and there is no single
   back edge to a single loop entry point, but only to the beginning
   of the Loop label. With Tarjan's DFS approach, we can identify the
   back-edge to the Loop label, and we are able to detect two
   exit-edges correctly.
 */
int loop_always_linear(int p, int k) {
  int i = 0;
  if (p > 0) {
    goto Loop;
  }

  while (i < k) {
  Loop:
    i++;
  }
  return 1;
}

int jump_inside_loop_constant_linear(int p, int k) {
  int i = 0;
  if (p > 0) {
    goto Loop;
  } else {
    return p;
  }
  while (i < k) {
  Loop:
    i++;
  }
  return 1;
}
