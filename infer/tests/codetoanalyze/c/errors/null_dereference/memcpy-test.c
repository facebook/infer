/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

struct X {
  int f;
};

typedef struct X X;

void testOK() {
  X x;
  X* p = malloc(sizeof(X));
  if (p)
    memcpy(p, &x, sizeof(X));
  free(p);
}

void testError1() {
  X x;
  X* p = 0;
  memcpy(p, &x, sizeof(X)); // crash
}

void testError2() {
  X x;
  X* r;
  X* p = 0;
  r = p;
  memcpy(r, &x, sizeof(X)); // crash
}

void testError3() {
  X* x = 0;
  X* p = malloc(sizeof(X));
    if (p) {
    memcpy(p, x, sizeof(X)); // crash
        free(p);
    }
}
