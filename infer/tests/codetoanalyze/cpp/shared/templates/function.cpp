/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace function {

struct X1 {
  int getVal() { return 1; }
};

struct X2 {
  int getVal() { return 0; }
};

struct X3 {
  int get() { return 0; }
};

template <class T>
int getVal(T& x) {
  return x.getVal();
}

// explicit specialization
template <>
int getVal(X3& x) {
  return x.get();
}

template <class T>
int createAndGetVal() {
  T x;
  return getVal(x);
}

template <class T>
int createAndDiv() {
  return 1 / createAndGetVal<T>();
}

// explicit instantiaion - will report div by 0
template int createAndDiv<X3>();

// explicit instantiation - won't report div by 0
template int createAndDiv<X1>();

// no instantiation for X2 - won't report div by 0
// if line below is not commented, infer would report
// division by 0
// template int createAndDiv<X2>();

int div0_get_val() {
  X1 x1;
  X3 x3;
  return getVal(x1) / getVal(x3);
}

int div1_get_val() {
  X1 x1;
  X3 x3;
  return getVal(x3) / getVal(x1);
}

int div0_create_and_get_val() {
  return createAndGetVal<X1>() / createAndGetVal<X3>();
}

int div1_create_and_get_val() {
  return createAndGetVal<X3>() / createAndGetVal<X1>();
}
} // namespace function
