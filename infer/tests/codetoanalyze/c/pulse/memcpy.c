/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

struct X {
  int f;
};

typedef struct X X;

void memcpy_ok() {
  X x;
  X* p = malloc(sizeof(X));
  if (p)
    memcpy(p, &x, sizeof(X));
  free(p);
}

void memcpy_to_null_bad() {
  X x;
  X* p = NULL;
  memcpy(p, &x, sizeof(X)); // crash
}

void memcpy_to_null_indirect_bad() {
  X x;
  X* r;
  X* p = NULL;
  r = p;
  memcpy(r, &x, sizeof(X)); // crash
}

void memcpy_from_null_bad() {
  X* src = NULL;
  X* p = malloc(sizeof(X));
  if (p) {
    memcpy(p, src, sizeof(X)); // crash
    free(p);
  }
}
