/*
 * Copyright (c) 2016 - present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

void nested_loop_with_label() {
  int i, j = 0;
  char a[10];

  for (i = 0; i < 10; i++) {
  outer_loop:
    a[j] = 'a'; /* BUG */
    for (j = 0; j <= 10; j++) {
      if (j >= 10)
        goto outer_loop;
    }
  }
}
