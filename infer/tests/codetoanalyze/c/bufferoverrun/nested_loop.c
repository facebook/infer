/*
 * Copyright (c) 2016-present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
