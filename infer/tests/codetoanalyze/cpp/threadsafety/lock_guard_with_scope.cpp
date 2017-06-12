/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

    // FIXME: missing unlocks in destructors make the following accesses
    // to be treated as protected
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
    // FIXME: It does not report due to missing unlocks in destructors
    result = suspiciously_written;
    return result;
  }

  // FIXME: It reports due to missing unlocks in destructors
  int get3() { return not_guarded; }

  int get4() { return suspiciously_read; }

 private:
  int well_guarded;
  int suspiciously_read;
  int suspiciously_written;
  int not_guarded;
  std::mutex mutex_;
};
}
