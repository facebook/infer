/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace dereferencing {

struct B {
  int c;
};

struct A {
  B b;
};

struct X {
  int w;
  int u;
  X* x1;
  X** x2;
  A a;
};

class Basic {
 public:
  Basic() {}

  void pointer_deref_race(int* v1) { (*v1)++; } // HIL: *(v1) := *(v1) + 1

  void pointer_arith_ok(int* v2) { v2++; } // HIL: v2 := v2 + 1

  void value_ok(int v3) { v3++; } // HIL: v3 := v3 + 1

  void field_race(int& f) { f++; } // HIL: *(f) := *(f) + 1

  void mixed_deref_race(X& xparam) {
    xparam.x1->w++; // HIL: xparam->x1->w := xparam->x1->w + 1
    (*xparam.x1).u++; // HIL: xparam->x1->u := xparam->x1->u + 1
    (**xparam.x2).a.b.c++; // HIL:*(xparam->x2)->a.b.c:= *(xparam->x2)->a.b.c+1
  }

  void call1() {
    pointer_deref_race(&p); // race - FalseNegative
    pointer_arith_ok(&q); // no race
    value_ok(h); // no race
    field_race(g); // race - FalseNegative
    mixed_deref_race(x); // race
  }

  int test_lock() {
    mutex_.lock();
    call1();
  }

  int test_unlock() { call1(); }

 private:
  int g;
  int h;
  int p;
  int q;
  X x;
  std::mutex mutex_;
};
} // namespace dereferencing
