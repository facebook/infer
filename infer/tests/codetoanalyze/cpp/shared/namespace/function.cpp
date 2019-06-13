/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace f1 {
int get() { return 1; }
int get0() { return 0; }
} // namespace f1

namespace f2 {
int get() { return -1; }
} // namespace f2

int div0_using() {
  using namespace f1;
  return 1 / get0();
}

int div0_namespace_resolution() { return 1 / (f1::get() + f2::get()); }

namespace f3 {
class C {
 public:
  static int ret_zero() { return 0; }
};
} // namespace f3

int using_div0() {
  using f3::C;
  return 1 / C::ret_zero();
}

int type_alias_div0() {
  using my_int = int;
  my_int x = 0;
  return 1 / x;
}
