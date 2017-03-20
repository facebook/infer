/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

/* FIXME: this test doesn't work with stdlibc++ headers
int impossible_npe(std::vector<X> in) {
  int* x = nullptr;
  for (auto iter = in.begin(); iter != in.end(); ++iter) {
    if (iter->id > 0 && iter->id <= 0) {
      return *x;
    }
  }
  return 1;
}
*/
}
