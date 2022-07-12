/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <utility>
#include <tuple> // for std::get

namespace pair {

std::pair<int, int*> pairOfZeroNull() {
  return std::pair<int, int*>(0, nullptr);
}

std::pair<int, int*> pairOfZeroNull2() { return std::make_pair(0, nullptr); }

int FN_deref_pair_null0_bad() {
  auto p = pairOfZeroNull();
  // Should report an NPE here as p.second is NULL
  return p.first + *p.second;
}

int FN_deref_pair_null1_bad() {
  auto p = pairOfZeroNull();
  // Should report an NPE here as p.second is NULL
  return std::get<0>(p) + *std::get<1>(p);
}

// there are reported as STACK_VARIABLE_ADDRESS_ESCAPE
// should report an NPE here as p.second is NULL
int deref_pair_null3_bad() {
  auto p = pairOfZeroNull2();
  return p.first + *p.second;
}

int deref_pair_null_guard_ok() {
  auto p = pairOfZeroNull();
  if (p.second != nullptr) {
    // Should not report an NPE here as p.second is guarded
    return p.first + *p.second;
  }
  return 0;
}
} // namespace pair
