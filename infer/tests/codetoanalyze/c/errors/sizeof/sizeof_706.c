/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
int sizeof_eval_good(void) {
  int a = 4;
  int b = sizeof(a);
  char c[2];

  if (a % 4) {
    return a / 0;
  }
  if (b % sizeof(a)) {
    return a / 0;
  }
  if (sizeof(c) > 2) {
    return a / 0;
  }
  if ((sizeof(c) / sizeof(c[0])) != 2) {
    return a / 0;
  }
  return 0;
}

void sentinel_bad(void) { return 1 / 0; }
