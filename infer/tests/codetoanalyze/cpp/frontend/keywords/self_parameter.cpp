/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class A {
 public:
  int meth_with_self(int self, int b) { return self + b; }
};

int fun_with_self(int self) { return self; }

int test(A* a) { return a->meth_with_self(1, 2) + fun_with_self(10); }
