/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <vector>

namespace iterator_access {
struct X {
  int id;
};

int possible_npe(std::vector<X> in) {
  int* x = nullptr;
  for (auto iter = in.begin(); iter != in.end(); ++iter) {
    if (iter->id >= 0 && iter->id <= 0) {
      return *x;
    }
  }
  return 1;
}

int impossible_npe(std::vector<X> in) {
  int* x = nullptr;
  for (auto iter = in.begin(); iter != in.end(); ++iter) {
    if (iter->id > 0 && iter->id <= 0) {
      return *x;
    }
  }
  return 1;
}
} // namespace iterator_access
