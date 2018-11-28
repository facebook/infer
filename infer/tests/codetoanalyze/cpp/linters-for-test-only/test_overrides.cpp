/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct A {
  virtual void foo();
  virtual void bar();
  void bazoo();
};

struct B : public A {
  void foo() override;
  void bar(int i);
  void bar() /* override */;
  void bazoo();
};
