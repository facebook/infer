/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
