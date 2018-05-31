/*
 * Copyright (c) 2016-present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void goto_loop() {
  int i = 0;
  char a[10];

loop_start:
  if (i >= 10)
    goto loop_end;
  a[i] = 'a'; /* SAFE */
  i++;
  goto loop_start;
loop_end:
  a[i] = 'a'; /* BUG */
}
