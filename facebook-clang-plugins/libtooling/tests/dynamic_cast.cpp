/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct A {
  virtual void f() {}
};
struct B : public A {};
struct C {};

void f() {
  A a;
  B b;

  A *ap = &b;
  B *b1 = dynamic_cast<B *>(&a);
  B *b2 = dynamic_cast<B *>(ap);
  C *c = dynamic_cast<C *>(ap);

  A &ar = dynamic_cast<A &>(*ap);
  B &br = dynamic_cast<B &>(*ap);
  C &cr = dynamic_cast<C &>(*ap);
}
