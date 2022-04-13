/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <string>
#include <vector>

struct A {
  std::vector<int> vec;
};

A& get_a_ref() {
  static A static_a;
  return static_a;
}

std::vector<int> copy_decl_bad() {
  auto cpy = get_a_ref(); // unnecessary copy, use a ref
  // call to copy constructor A::A(a, n$0)
  return cpy.vec;
}
