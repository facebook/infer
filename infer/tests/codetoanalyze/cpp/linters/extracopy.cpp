/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <iostream>
#include <map>

struct A {
  int a;
};

const A& get_a_ref() {
  static A a;
  return a;
}

A get_a() {
  A a;
  return a;
}

int test_a() {
  static auto x = get_a_ref();
  auto y = get_a_ref();
  auto copy_y = get_a_ref();
  auto z = get_a();
  return 0;
}

auto global_a = get_a_ref();

void test_map() {
  std::map<int, int> intMap{{1, 2}, {3, 4}};
  for (auto p : intMap) {
    std::cout << p.first << "->" << p.second << std::endl;
  }
  for (auto copy_p : intMap) {
    std::cout << copy_p.first << "->" << copy_p.second << std::endl;
  }
  for (const auto& p : intMap) {
    std::cout << p.first << "->" << p.second << std::endl;
  }
}

int helper_ce(int x) { return 2 * x; }

int test_const_exp(int x) {
  int i = helper_ce(x); // notconst exp
  int j = helper_ce(2); // not const exp
  int k = 2 * 3; // const exp
  int y = x + x; // not const exp
  return 0;
}
