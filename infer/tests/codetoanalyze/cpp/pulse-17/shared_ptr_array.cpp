/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

namespace shared_ptr_array {

void array_destructor_ok() { std::shared_ptr<int[]> x(new int[5]); }

int array_destructor_bad() {
  auto p = new int[5];
  { std::shared_ptr<int[]> x(p); }
  return p[0];
}

} // namespace shared_ptr_array
