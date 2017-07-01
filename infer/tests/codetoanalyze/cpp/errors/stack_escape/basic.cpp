/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
