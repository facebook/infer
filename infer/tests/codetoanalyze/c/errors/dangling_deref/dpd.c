/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdio.h>
#include <stdlib.h>

int* set42(int* x) {
  *x = 42;
  return x;
}

void nodpd() {
  int w, *z;
  z = set42(&w);
}

void nodpd1() {
  int* y = malloc(sizeof(int));
  int* z;
  z = set42(y);
  free(y);
}

void dpd() {
  int* y;
  int* z;
  z = set42(y);
}

void intraprocdpd() {
  int* y;
  int* z;
  *y = 42;
  z = y;
}
