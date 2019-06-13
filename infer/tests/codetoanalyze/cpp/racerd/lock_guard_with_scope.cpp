/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace basics {

class LockGuardWithScope {
 public:
  LockGuardWithScope() {}

  void set(int new_value) {
    {
      std::lock_guard<std::mutex> lock(mutex_);
      well_guarded = new_value;
      suspiciously_read = new_value;
    }

    not_guarded = new_value;
    suspiciously_written = new_value;
  }

  int get1() {
    int result;
    std::lock_guard<std::mutex> lock(mutex_);
    result = well_guarded;
    return result;
  }

  int get2() {
    int result;
    std::lock_guard<std::mutex> lock(mutex_);
    result = suspiciously_written;
    return result;
  }

  int get3() { return not_guarded; }

  int get4() { return suspiciously_read; }

 private:
  int well_guarded;
  int suspiciously_read;
  int suspiciously_written;
  int not_guarded;
  std::mutex mutex_;
};
} // namespace basics
