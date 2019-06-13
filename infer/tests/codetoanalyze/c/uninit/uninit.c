/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef struct A {
  int field;
} B;

typedef B (*entry_fn)();

int test_higher_order(entry_fn f) {
  B event = f();
  return event.field;
}

int dereference_bad() {
  int* p;
  return *p;
}

void self_assign_bad() {
  int x;
  x = x;
}

void use_and_mayinit(int, int*);

void call_to_use_and_mayinit_bad_FN() {
  int x;
  use_and_mayinit(x, &x);
}
