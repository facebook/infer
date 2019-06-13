/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace method {

struct X1 {
  int get() { return 1; }
};

struct X2 {
  int get() { return 0; }
};

struct X3 {
  int get() { return 0; }
};

struct Getter {
  template <class S>
  int get(S& s) {
    return s.get();
  }
};

template <class T>
struct GetterTempl {
  template <class S>
  int get(T& t, S& s) {
    return t.get() + s.get();
  }
};

int div0_getter() {
  X2 x2;
  Getter g;
  return 1 / g.get(x2);
}

int div1_getter() {
  X1 x1;
  Getter g;
  return 1 / g.get(x1);
}

int div0_getter_templ() {
  X2 x2;
  X3 x3;
  GetterTempl<X3> g;
  return 1 / g.get(x3, x2);
}

int div0_getter_templ2() {
  X2 x2_1;
  X2 x2_2;
  GetterTempl<X2> g;
  return 1 / g.get(x2_1, x2_2);
}

int div1_getter_templ() {
  X1 x1;
  X2 x2;
  GetterTempl<X2> g;
  return 1 / g.get(x2, x1);
}

int div1_getter_templ2() {
  X1 x1_1;
  X1 x1_2;
  GetterTempl<X1> g;
  return 1 / g.get(x1_1, x1_2);
}
} // namespace method
