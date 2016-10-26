/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

struct X {
  int f;
  int call() { return f; }
};

X global;
X* get_ptr() { return &global; }
X& get_ref() { return global; }

void test_ref() {
  int f = get_ref().f;
  int c = get_ref().call();
}

void test_ptr() {
  int f = get_ptr()->f;
  int c = get_ptr()->call();
}
