/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class Base {
 public:
  int fun() { return 1; }
  int fun_redefine() { return 10; } // note that they are not virtual
};

class Sub : public Base {
 public:
  int fun_redefine() { return 20; }
};

void call_static_methods() {
  Base* b = new Base;
  Base* s1 = new Sub; // note the type of s1
  Sub* s2 = new Sub;

  b->fun();
  s1->fun();
  s2->fun();

  b->fun_redefine();
  s1->fun_redefine(); // will call Base::fun_redefine
  s2->fun_redefine(); // will call Sub::fun_redefine
}
