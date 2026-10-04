/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <list>
#include <map>

int map_iterate_pure(const std::map<int, int>& map) {
  int sum = 0;
  for (const auto& entry : map) {
    sum += entry.second;
  }
  return sum;
}

int map_find_pure(const std::map<int, int>& map) {
  auto it = map.find(1);
  return it == map.end() ? 0 : it->second;
}

void map_erase_impure(std::map<int, int>& map) { map.erase(map.begin()); }

// the elements created by the models exist only in the post, so writes through
// them are not seen as modifications
void FN_list_write_elements_impure(std::list<int>& list) {
  for (auto& x : list) {
    x = 0;
  }
}

void FN_map_write_found_impure(std::map<int, int>& map) {
  auto it = map.find(1);
  if (it != map.end()) {
    it->second = 2;
  }
}
