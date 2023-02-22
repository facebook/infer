/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <vector>

template <typename X>
void copy_in_header_bad(const std::vector<X>& arg) {
  auto c = arg; // creates a copy
}
