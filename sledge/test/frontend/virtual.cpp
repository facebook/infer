/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
