/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <map>
#include <string>
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

void write_string_map_ok(std::map<int, std::string> map) {
  int source = __infer_taint_source();
  map[source] = "string";
}

int read_map_ok(std::map<int, int> map) {
  int source = __infer_taint_source();
  return map[source];
}

} // namespace vectors
