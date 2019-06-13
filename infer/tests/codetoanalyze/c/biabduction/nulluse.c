/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

void (*global)(void**);

struct T {
  void (*go)();
};

struct S {
  struct T* t;
  void* x;
};

void doit(void** x) {
  if (global != 0) {
    global(x);
  }
}

void f(struct S* s) {
  if (s == 0)
    return;
  doit(&s->x);
  if (s->t->go) { // should *not* give NULL_TEST_AFTER_DEREFERENCE
    s->t->go();
  }
}

void fail(int* x) {
  if (x == 0) {
    *x = 1; // should give NULL_DEREFERENCE
  }
}

int main() {}
