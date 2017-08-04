/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
