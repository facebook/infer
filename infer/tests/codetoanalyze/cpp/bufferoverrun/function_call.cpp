/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

struct S {
  int field;
};

void ref_set_to_zero(int& x) {
  x = 0;
}

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
