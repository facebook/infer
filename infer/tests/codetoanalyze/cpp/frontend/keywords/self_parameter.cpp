/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class A {
 public:
  int meth_with_self(int self, int b) { return self + b; }
};

int fun_with_self(int self) { return self; }

int test(A* a) { return a->meth_with_self(1, 2) + fun_with_self(10); }
