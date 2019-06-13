/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
typedef struct {
  int i;
} Foo;

static int variable_init_ok(const int* x) {
  const Foo* foo = (const Foo*)x;
  return foo->i;
}

Foo* variable_init_bad(const int* x) {
  Foo* foo = (const Foo*)x; // aliasing to x
  foo->i = 0;
  return foo;
}

void set_fresh_ok() {
  Foo* foo = {0};
  foo->i = 0;
}

void set_fresh_primitive_ok(int x) {
  Foo* foo = {x};
  foo->i = 0;
}

void set_alias_primitive_bad(int* x) {
  Foo* foo = {x};
  foo->i = 0;
}
