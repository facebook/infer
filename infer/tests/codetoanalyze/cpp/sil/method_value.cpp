/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct C {
  void method() {}
  static int static_method(int x) { return x; }
};

void method_values() {
  void (C::*method)() = &C::method;
  int (*static_method)(int) = &C::static_method;
  int (*static_method_no_address)(int) = C::static_method;
}
