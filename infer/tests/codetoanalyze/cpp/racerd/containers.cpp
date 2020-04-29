/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>
#include <map>

namespace containers {

struct A {
  int value;
};

struct B {

  void FN_write_container_bad(int key, int value) {
    mutex_.lock();
    map[key].value = value;
    mutex_.unlock();
  }

  int FN_get_bad(int key) { return map[key].value; }

  int FN_size_bad() { return map.size(); }

 private:
  std::map<int, A> map;
  std::mutex mutex_;
};
} // namespace containers
