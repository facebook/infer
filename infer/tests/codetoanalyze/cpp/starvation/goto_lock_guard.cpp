/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace goto_lock_guard {

class GotoReleasesLock {
 public:
  GotoReleasesLock() {}

  // the goto releases mutex_1 before mutex_2 is taken
  void thread1_ok(bool b) {
    {
      std::lock_guard<std::mutex> lock1(mutex_1);
      if (b) {
        goto out;
      }
    }
    return;
  out:
    std::lock_guard<std::mutex> lock2(mutex_2);
  }

  void thread2_ok() {
    std::lock_guard<std::mutex> lock2(mutex_2);
    std::lock_guard<std::mutex> lock1(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};
} // namespace goto_lock_guard
