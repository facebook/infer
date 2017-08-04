/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <map>
#include <vector>

extern int __infer_taint_source();

namespace vectors {

void write_vector_bad(std::vector<int> vec) {
  int source = __infer_taint_source();
  vec[source] = 2;
}

int read_vector_bad(std::vector<int> vec) {
  int source = __infer_taint_source();
  return vec[source];
}

// don't care about map accesses
void write_map_ok(std::map<int, int> map) {
  int source = __infer_taint_source();
  map[source] = 2;
}

void read_map_ok(std::map<int, int> map) {
  int source = __infer_taint_source();
  return map[source];
}

} // namespace vectors
