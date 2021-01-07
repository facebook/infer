/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int simple_for_loop() {
  int j = 0;
  for (int i = 0; i < 10; i++) {
    j += j;
  }
  return 0;
}

void for_loop_no_brackets() {
  int j = 0;
  for (int i = 0; i < 1; i++)
    j;
}
