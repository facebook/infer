/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <limits>

namespace consteval_call {

consteval int int_digits() { return std::numeric_limits<int>::digits; }

void immediate_invocation_ok() {
  int* p = nullptr;
  if (int_digits() != 31) {
    *p = 1;
  }
}

void immediate_invocation_bad() {
  int* p = nullptr;
  if (int_digits() == 31) {
    *p = 1;
  }
}

} // namespace consteval_call
