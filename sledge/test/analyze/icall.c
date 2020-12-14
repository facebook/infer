/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>

typedef struct _ {
  int a;
  int b;
  int c;
} S;

typedef int* (*F)(S*);

int* g(S* x) { return &(x->b); }

int* h(S* x) { return &(x->c); }

void main() {
  S t;
  void* x;
  int* y;
  F f;

  f = &g;
  x = &(t.a);
  y = (*f)(x);
  assert(x != y);

  f = &h;
  x = &(t.a);
  y = (*f)(x);
  assert(x != y);
}
