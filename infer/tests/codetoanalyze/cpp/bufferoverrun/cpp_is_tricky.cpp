/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <vector>

namespace CppIsTricky {
void vector_size_Bad() {
  const auto vec = std::vector<int>{1, 2, 3};
  const int numExpectedElements = 1;
  const auto delta = numExpectedElements - vec.size();
}

void minus1_Bad_FN() {
  const unsigned long i2 = 18446744073709551614u;
  const unsigned long i1 = 446744073709551614u;
  const int d1 = i2 - i1;
}
void minus2_Bad_FN() {
  const unsigned long i2 = 18446744073709551614u;
  const unsigned long i1 = 446744073709551614u;
  const long d2 = i2 - i1;
}
void minus3_Good() {
  const unsigned long i2 = 18446744073709551614u;
  const unsigned long i1 = 446744073709551614u;
  const auto d3 = i2 - i1;
}
} // namespace CppIsTricky
