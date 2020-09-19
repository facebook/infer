/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct S {
 public:
  S() {} // User defined constructor makes S non-POD.
  ~S() {} // User defined destructor makes it non-trivial.
};
void test() {
  const S &s_ref = S(); // Requires a CXXBindTemporaryExpr.
}
