/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace struct_forward_declare {

// this will never be defined
struct Y;

// this will be defined at the end
struct Z;

struct X;
struct X {
  int f;
  int getF() { return f; }
  Y* y;
  Z* z;
};

void fun_with_Z(Z* z1) { Z* z2 = z1; }

struct Z {
  int f;
  int getF() { return f; }
};

// forward declare again
struct X;

int X_div0() {
  X x;
  x.f = 0;
  return 1 / x.getF();
}

int X_ptr_div0(X* x) {
  x->f = 0;
  return 1 / x->getF();
}

int X_Y_div0() {
  X x;
  x.y = nullptr;
  x.f = 0;
  if (x.y) {
    return 1;
  }
  return 1 / x.getF();
}

int Z_div0() {
  Z z;
  z.f = 0;
  return 1 / z.getF();
}

int Z_ptr_div0(Z* z) {
  // internal implementation details, subject to change:
  // Z * will by Sil.Tptr (Sil.Tvar Z) type and
  // will get expanded by clang frontend
  z->f = 0;
  return 1 / z->getF();
}
} // namespace struct_forward_declare
