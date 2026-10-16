/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstdio>

namespace fclose_test {

void std_fclose_nullptr_bad() { std::fclose(nullptr); }

void std_fopen_check_fclose_ok(const char* path) {
  if (std::FILE* f = std::fopen(path, "r")) {
    std::fclose(f);
  }
}

} // namespace fclose_test
