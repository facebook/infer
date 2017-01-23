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

void nested_loop() {
  int i, j;
  char a[10];

  for (i = 0; i < 10; i++) {
    a[i] = 'a'; /* SAFE */
    for (j = 0; j <= 10; j++) {
      a[j] = 'a'; /* BUG */
    }
  }
}
