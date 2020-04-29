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

  void FN_pointer_deref_bad(int b) {
    if (b) {
      pointer_deref(&p);
    } else {
      mutex_.lock();
      pointer_deref(&p);
      mutex_.unlock();
    }
  }

  void pointer_arith_ok(int b) {
    if (b) {
      pointer_arith(&q);
    } else {
      mutex_.lock();
      pointer_arith(&q);
      mutex_.unlock();
    }
  }

  void value_ok(int b) {
    if (b) {
      value(h);
    } else {
      mutex_.lock();
      value(h);
      mutex_.unlock();
    }
  }

  void FN_field_bad(int b) {
    if (b) {
      field(g);
    } else {
      mutex_.lock();
      field(g);
      mutex_.unlock();
    }
  }

  void deref_w_bad(int b) {
    if (b) {
      deref_w(x);
    } else {
      mutex_.lock();
      deref_w(x);
      mutex_.unlock();
    }
  }

  void deref_u_bad(int b) {
    if (b) {
      deref_u(x);
    } else {
      mutex_.lock();
      deref_u(x);
      mutex_.unlock();
    }
  }

  void deref_abc_bad(int b) {
    if (b) {
      deref_abc(x);
    } else {
      mutex_.lock();
      deref_abc(x);
      mutex_.unlock();
    }
  }

 private:
  void pointer_deref(int* v1) { (*v1)++; } // HIL: *(v1) := *(v1) + 1

  void pointer_arith(int* v2) { v2++; } // HIL: v2 := v2 + 1

  void value(int v3) { v3++; } // HIL: v3 := v3 + 1

  void field(int& f) { f++; } // HIL: *(f) := *(f) + 1

  void deref_w(X& xparam) {
    xparam.x1->w++; // HIL: xparam->x1->w := xparam->x1->w + 1
  }

  void deref_u(X& xparam) {
    (*xparam.x1).u++; // HIL: xparam->x1->u := xparam->x1->u + 1
  }

  void deref_abc(X& xparam) {
    (**xparam.x2).a.b.c++; // HIL:*(xparam->x2)->a.b.c:= *(xparam->x2)->a.b.c+1
  }

  int g;
  int h;
  int p;
  int q;
  X x;
  std::mutex mutex_;
};
} // namespace dereferencing
