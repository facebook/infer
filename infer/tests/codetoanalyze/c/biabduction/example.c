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

void local_addr_noalias_ok(int* p) {
  int* q = 0;
  int x = 1;
  if (&x == p) {
    *q = 42;
  }
}

void local_addr_noalias_bad(int* p) {
  int* q = 0;
  int x = 1;
  if (&x != p) {
    *q = 42;
  }
}

static int g = 0;
void global_addr_alias_bad(int* p) {
  int* q = 0;
  if (&g == p) {
    *q = 42;
  }
}

int bar() {
  /* The division by zero should be found but filtered out by default */
  return 1 / 0;
}
