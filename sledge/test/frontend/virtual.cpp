/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class B {
public:
  virtual void f() { g(); };
  virtual void g(){};
};

class C : public B {
public:
  virtual void f() { B::f(); };
  virtual void g(){};
};

int main() {
  B* p = new C;
  p->f();
  return 0;
}
