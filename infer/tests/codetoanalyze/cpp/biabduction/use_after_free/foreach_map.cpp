/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <map>
#include <vector>
#include <iostream>
#include <unordered_map>

struct Y {
  int f;
  ~Y(){};
};

namespace use_after_free {
class Basic {
 public:
  Basic() {}

  std::unordered_map<int, Y*> umap_ys;
  std::map<int, Y*> map_ys;
  std::vector<std::pair<int, Y*>> pairs_ys;
  std::vector<Y*> ys;

  void test_delete_ok() {
    Y* p = new Y();
    delete p;
  }

  void test_double_delete_bad() {
    Y* p = new Y();
    delete p;
    delete p; // error
  }

  void test_for_vector_delete_ok() {
    for (auto& y : ys) {
      delete y;
    }
  }

  void test_for_pairs_delete_ok() {
    for (auto& y : pairs_ys) {
      delete y.second;
    }
  }

  // TODO: missing model of std::__hash_map_iterator,void*>*>>_operator++
  void test_for_map_delete_ok_FP() {
    for (auto& y : map_ys) {
      delete y.second; // error
    }
  }

  // TODO: missing model of std::__hash_map_iterator,void*>*>>_operator++
  void test_for_umap_delete_ok_FP() {
    for (auto& y : umap_ys) {
      delete y.second; // error
    }
  }

  void test_for_umap_delete_break_ok() {
    for (auto& y : umap_ys) {
      delete y.second;
      break;
    }
  }
};
} // namespace use_after_free
