/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct X {
  // operator which is a method
  int operator[](int x) { return x; }
};

struct Y : public X {};

// operator that is a function
int operator*(const X& x1, int v) { return v; }

int div0_method_op(X& x) {
  // call method operator
  int v = x[0];
  return 1 / v;
}

int div0_method_op_ptr(X* x) { return 1 / (*x)[0]; }

int div0_function_op(X& x) {
  // call function operator
  int v = x * 0;
  return 1 / v;
}

int div0_method(X& x) {
  // call method operator as a method
  int v = x.operator[](0);
  return 1 / v;
}

int div0_inheritted_op(Y& y) {
  // call operator of superclass
  return 1 / y[0];
}

int div1_method_op(X& x) { return 1 / x[1]; }
