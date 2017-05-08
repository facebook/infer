/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

void foo() {
  int* p = 0;
  *p = 42;
}

int bar() {
  /* The division by zero should be found but filtered out by default */
  return 1 / 0;
}
