/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace reference_field {

struct X {
  int f;
};

// Compare cfgs for different field types.
// Ptr and Ref translation should be the same modulo pointer type

// Ref stores reference fields
// Ptr stores pointer fields and has same semantics as Ref
// Val stores value fields and has different semantics than Ref and Ptr

struct Ref {
  X& x;
  int& i;
  Ref(X& r_) : x(r_), i(x.f) {}
  int getF() { return x.f; }
  int getI() { return i; }
};

struct Ptr {
  X* x;
  int* i;
  Ptr(X& r_) : x(&r_), i(&x->f) {}
  int getF() { return x->f; }
  int getI() { return *i; }
};

struct Val {
  X x;
  int i;
  Val(X& r_) : x(r_), i(x.f) {}
  int getF() { return x.f; }
  int getI() { return i; }
};

// Ref Tests
int ref_F_div0() {
  X x;
  x.f = 1;
  Ref r(x);
  x.f = 0;
  return 1 / r.x.f;
}

int ref_I_div0() {
  X x;
  x.f = 1;
  Ref r(x);
  x.f = 0;
  return 1 / r.i;
}

int ref_getF_div0() {
  X x;
  x.f = 1;
  Ref r(x);
  x.f = 0;
  return 1 / r.getF();
}

int ref_getI_div0() {
  X x;
  x.f = 1;
  Ref r(x);
  x.f = 0;
  return 1 / r.getI();
}

// Ptr Tests
int ptr_F_div0() {
  X x;
  x.f = 1;
  Ptr r(x);
  x.f = 0;
  return 1 / r.x->f;
}

int ptr_I_div0() {
  X x;
  x.f = 1;
  Ptr r(x);
  x.f = 0;
  return 1 / *r.i;
}

int ptr_getF_div0() {
  X x;
  x.f = 1;
  Ptr r(x);
  x.f = 0;
  return 1 / r.getF();
}

int ptr_getI_div0() {
  X x;
  x.f = 1;
  Ptr r(x);
  x.f = 0;
  return 1 / r.getI();
}

// Val tests
int val_F_div0() {
  X x;
  x.f = 0;
  Val r(x);
  x.f = 1;
  return 1 / r.x.f;
}

int val_I_div0() {
  X x;
  x.f = 0;
  Val r(x);
  x.f = 1;
  return 1 / r.i;
}

int val_getF_div0() {
  X x;
  x.f = 0;
  Val r(x);
  x.f = 1;
  return 1 / r.getF();
}

int val_getI_div0() {
  X x;
  x.f = 0;
  Val r(x);
  x.f = 1;
  return 1 / r.getI();
}
} // namespace reference_field
