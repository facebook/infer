/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <mutex>
#include <map>

namespace containers {

struct A {
  int value;
};

struct B {

  void access_container_and_contents_ok(int key, int value) {
    int s = map.size();
    mutex_.lock();
    map[key].value = value;
  }

 private:
  std::map<int, A> map;
  std::mutex mutex_;
};
} // namespace containers
