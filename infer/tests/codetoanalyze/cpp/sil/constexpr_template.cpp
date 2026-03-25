/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int foo(int a);

template <int N>
struct X {
  bool isZero() { return N == 0; }
};

int use_template() {
  X<3> x;
  return x.isZero();
}

void use_constexpr_lambda() {
  constexpr int x = 10;
  [](int v) { foo(v); }(x);
}
