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

class UniqueLock {
 public:
  UniqueLock() {}

  void set(int new_value) {
    std::unique_lock<std::mutex> g(mutex_);
    well_guarded1 = new_value;
    suspiciously_read1 = new_value;
    g.unlock();
    not_guarded1 = new_value;
    suspiciously_written1 = new_value;
  }

  void set2(int new_value) {
    std::unique_lock<std::mutex> g(mutex_, std::defer_lock);
    not_guarded2 = new_value;
    suspiciously_written2 = new_value;
    g.lock();
    well_guarded2 = new_value;
    suspiciously_read2 = new_value;
    g.unlock();
  }

  int get1() {
    int result;
    std::lock_guard<std::mutex> lock(mutex_);
    result = well_guarded1;
    return result + well_guarded2;
  }

  int get2() {
    int result;
    std::lock_guard<std::mutex> lock(mutex_);
    result = suspiciously_written1;
    return result + suspiciously_written2;
  }

  int get3() {
    int result = not_guarded1;
    return result + not_guarded2;
  }

  int get4() {
    int result = suspiciously_read1;
    return result + suspiciously_read2;
  }

 private:
  int well_guarded1;
  int suspiciously_read1;
  int suspiciously_written1;
  int not_guarded1;
  int well_guarded2;
  int suspiciously_read2;
  int suspiciously_written2;
  int not_guarded2;
  std::mutex mutex_;
};
}
