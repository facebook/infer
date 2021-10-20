/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <type_traits>

void iterate_integral_constant(std::integral_constant<int, 5> con) {
  for (int i = 0; i < con; ++i) {
  }
}
