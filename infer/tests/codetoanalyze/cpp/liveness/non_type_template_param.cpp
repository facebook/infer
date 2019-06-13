/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
// see also https://github.com/facebook/infer/issues/950

template <int T>
struct X {
  bool isZeroBad() {
    int unused = 1;
    return T == 0;
  }
};

int instanciateTemplateBad() {
  X<3> x;
  x.isZeroBad();
  int unused = 1;
  return 0;
}

void instanciateTemplateConstOk_FP() {
  const int foo = 7;
  X<foo> x;
  x.isZeroBad();
}

void instanciateTemplateConstExprOk() {
  constexpr int foo = 7;
  X<foo> x;
  x.isZeroBad();
}
