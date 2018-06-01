/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int jump_inside_loop(int p) {
  int i = 0;
  if (p > 0) {
    goto Loop;
  } else {
    return p;
  }
  while (i < 500) {
  Loop:
    i++;
  }
  return 1;
}
