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

std::pair<int, int> pairOfZeroNull3() { return std::make_pair(58, 42); }

int deref_makepair_constants0_bad() {
  auto p = pairOfZeroNull3();
  if (p.first == 58) {
    // Should report an NPE here as p.first is equal to 58
    int* q = nullptr;
    return *q;
  }
  return p.first;
}

int deref_makepair_constants1_bad() {
  auto p = pairOfZeroNull3();
  if (p.second == 42) {
    // Should report an NPE here as p.second is equal to 42
    int* q = nullptr;
    return *q;
  }
  return p.first;
}

int deref_makepair_constants0_ok() {
  auto p = pairOfZeroNull3();
  if (p.first != 58) {
    // Should not report an NPE here as p.first is equal to 58
    int* q = nullptr;
    return *q;
  }
  return p.first;
}

int deref_makepair_constants1_ok() {
  auto p = pairOfZeroNull3();
  if (p.second != 42) {
    // Should not report an NPE here as p.second is equal to 42
    int* q = nullptr;
    return *q;
  }
  return p.first;
}

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

int FN_deref_makepair_null0_bad() {
  auto p = pairOfZeroNull2();
  // Should report an NPE here as p.second is NULL
  return p.first + *p.second;
}

int FN_deref_makepairnull1_bad() {
  auto p = pairOfZeroNull2();
  // Should report an NPE here as p.second is NULL
  return std::get<0>(p) + *std::get<1>(p);
}

int deref_pair_null_guard0_ok() {
  auto p = pairOfZeroNull();
  if (p.second != nullptr) {
    // Should not report an NPE here as p.second is guarded
    return p.first + *p.second;
  }
  return 0;
}

int deref_makepair_null_guard1_ok() {
  auto p = pairOfZeroNull2();
  if (p.second != nullptr) {
    // Should not report an NPE here as p.second is guarded
    return p.first + *p.second;
  }
  return 0;
}

} // namespace pair
