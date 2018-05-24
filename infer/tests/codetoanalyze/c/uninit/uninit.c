/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
