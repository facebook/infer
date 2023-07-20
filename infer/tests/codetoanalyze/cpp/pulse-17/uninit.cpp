/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <map>

namespace uninit {
class Value {
 public:
  int fld1;
  int fld2;
};

void structured_binding_ok(const std::map<int, Value>& m) {
  for (const auto& [key, value] : m) {
    int v = value.fld1;
  }
};
} // namespace uninit
