/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>
#include <vector>

std::shared_ptr<std::vector<int>> get() {
  std::vector<int> v = {1, 2, 3};
  return std::make_shared<std::vector<int>>(v);
}

int main() {
  int n;
  for (auto& t : *get()) {
    n += t;
  }
  return n;
}
