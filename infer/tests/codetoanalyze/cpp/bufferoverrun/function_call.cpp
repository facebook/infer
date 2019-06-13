/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct S {
  int field;
};

void ref_set_to_zero(int& x) { x = 0; }

void struct_ref_set_to_zero(struct S& s) { s.field = 0; }

void call_by_ref_good() {
  int arr[10];
  int i = 99;
  ref_set_to_zero(i);
  arr[i] = 123;
}

void call_by_ref_bad() {
  int arr[10];
  int i = 5;
  ref_set_to_zero(i);
  arr[i - 1] = 123;
}

struct S init_S(int x) {
  struct S s = {x};
  return s;
}

int loop_with_init_S(int length) {
  int i = 0;
  while (i < length) {
    struct S s = init_S(i + 1);
    i = s.field;
  }
  return i;
}

void call_loop_with_init_S_Good() {
  int a[10];
  a[loop_with_init_S(5)] = 0;
}

void call_loop_with_init_S_Bad() {
  int a[10];
  a[loop_with_init_S(10)] = 0;
}
