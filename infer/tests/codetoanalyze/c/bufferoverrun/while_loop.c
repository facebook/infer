/*
 * Copyright (c) 2016-present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void while_loop() {
  int i = 0;
  char* a = malloc(10);
  while (*(a + i) && i < 10) /* BUG */
    a[i++] = 1; /* SAFE */
}
