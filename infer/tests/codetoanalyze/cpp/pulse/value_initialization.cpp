/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace value_initialization {

struct S {
  int x;
};

struct Agg {
  S* p;
  int n;
};

int new_null_field_bad() {
  Agg* a = new Agg();
  int r = a->p->x;
  delete a;
  return r;
}

int temporary_null_field_bad() {
  Agg a = Agg();
  return a.p->x;
}

int zero_field_ok() {
  S s{0};
  Agg a = Agg();
  if (a.n != 0) {
    return a.p->x;
  }
  return s.x;
}

struct Defaulted {
  Defaulted() = default;
  S* p;
};

int defaulted_constructor_null_field_bad() {
  Defaulted d = Defaulted();
  return d.p->x;
}

struct Outer {
  Agg agg;
  S* q;
};

int nested_null_field_bad() {
  Outer o = Outer();
  return o.agg.p->x;
}

struct Derived : Agg {
  int m;
};

int base_null_field_bad() {
  Derived d = Derived();
  return d.p->x;
}

} // namespace value_initialization
