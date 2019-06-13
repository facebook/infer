/*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void nested_loop_with_label() {
  int i, j = 0;
  char a[10];

  for (i = 0; i < 10; i++) {
  outer_loop:
    a[j] = 'a'; /* BUG */
    for (j = 0; j <= 10; j++) { // Loop condition always true
      if (j >= 10)
        goto outer_loop;
    }
  }
}
