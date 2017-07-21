/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <utility>
#include <tuple> // for std::get

namespace pair {

std::pair<int, int*> pairOfZeroNull() {
  return std::pair<int, int*>(0, nullptr);
}

std::pair<int, int*> pairOfZeroNull2() { return std::make_pair(0, nullptr); }

int deref_pair_null0_bad() {
  auto p = pairOfZeroNull();
  // Should report an NPE here as p.second is NULL
  return p.first + *p.second;
}

int deref_pair_null1_bad() {
  auto p = pairOfZeroNull();
  // Should report an NPE here as p.second is NULL
  return std::get<0>(p) + *std::get<1>(p);
}

int deref_pair_null2_bad() {
  auto p = pairOfZeroNull();
  // Should report an NPE here as p.second is NULL
  return std::get<int>(p) + *std::get<int*>(p);
}

int deref_pair_null3_bad() {
  auto p = pairOfZeroNull2();
  // Should report an NPE here as p.second is NULL
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
}
