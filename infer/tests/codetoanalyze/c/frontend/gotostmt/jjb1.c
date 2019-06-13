/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

extern void print_int(int);

int jjb1() {
  int x, y;

  x++;
  if (x / 2) {
  L0:
    if (x <= 0)
      goto L3;
    x--;
    print_int(x);
    goto L1;
  } else {
  L1:
    y++;
    print_int(y);
    goto L0;
  }
L3:

  return 0;
}
