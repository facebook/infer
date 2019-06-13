/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int* basic_escape_local_bad() {
  int a;
  return &a;
}

int* basic_escape_param_bad(int a) { return &a; }

struct EscapeTest {
  int x;
};

int* escape_local_struct_member_bad() {
  EscapeTest esc;
  return &(esc.x);
}

struct A {
  A() {}
};

struct B {
  const A& FN_return_ref_bad() { return A(); }
};
