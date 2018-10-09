/*
 * Copyright (c) 2018-present, Facebook, Inc.
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

void FN_self_assign() {
  int x;
  x = x;
}
