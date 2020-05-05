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

  void not_guarded_ok(int b, int new_value) {
    if (b) {
      not_guarded = new_value;
    } else {
      return not_guarded;
    }
  }

  void well_guarded_ok(int b, int new_value) {
    std::lock_guard<std::mutex> lock(mutex_);
    if (b) {
      well_guarded = new_value;
    } else {
      return well_guarded;
    }
  }

  void suspiciously_read_bad(int b, int new_value) {
    if (b) {
      std::lock_guard<std::mutex> lock(mutex_);
      suspiciously_read = new_value;
    } else {
      return suspiciously_read;
    }
  }

  void suspiciously_written_ok(int b, int new_value) {
    if (b) {
      suspiciously_written = new_value;
    } else {
      std::lock_guard<std::mutex> lock(mutex_);
      return suspiciously_written;
    }
  }

 private:
  int well_guarded;
  int suspiciously_read;
  int suspiciously_written;
  int not_guarded;
  std::mutex mutex_;
};
} // namespace basics
