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

  int get() { return field_1; }

  int set(std::mutex& mutex, int data) {
    std::lock_guard<std::mutex> lock(mutex);
    field_1 = data;
  }

 private:
  int field_1;
};
} // namespace without_mutex
