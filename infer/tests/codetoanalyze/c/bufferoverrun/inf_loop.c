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

void inf_loop() {
  int i = 0;
  char a[10];

  while (1) {
    if (i >= 10)
      i = 0;
    a[i] = 'a'; /* SAFE */
    i++;
  }
}
