/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace init_statement {

class LockGuardInSwitchInit {
 public:
  int guarded_ok(int k) {
    switch (std::lock_guard<std::mutex> lock(mutex_); k) {
      case 0:
        guarded = 1;
        break;
      default:
        return guarded;
    }
    return 0;
  }

  int read_after_switch_bad(int k) {
    switch (std::lock_guard<std::mutex> lock(mutex_); k) {
      case 0:
        read_after_switch = 1;
        break;
      default:
        break;
    }
    return read_after_switch;
  }

 private:
  int guarded;
  int read_after_switch;
  std::mutex mutex_;
};

} // namespace init_statement
