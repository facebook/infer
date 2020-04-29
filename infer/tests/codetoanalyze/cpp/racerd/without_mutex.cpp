/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <mutex>

namespace without_mutex {

class WithoutMutex {
 public:
  WithoutMutex() {}

  int get_bad() { return field; }

  int set_bad(std::mutex& mutex, int data) {
    std::lock_guard<std::mutex> lock(mutex);
    field = data;
  }

 private:
  int field;
};
} // namespace without_mutex
