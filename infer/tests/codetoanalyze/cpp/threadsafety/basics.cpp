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

class Basic {
 public:
  Basic() {}

  void set(int new_value) {
    not_guarded = new_value;
    mutex_.lock();
    well_guarded = new_value;
    suspiciously_read = new_value;
    mutex_.unlock();
    suspiciously_written = new_value;
  }

  int get1() {
    int result;
    mutex_.lock();
    result = well_guarded;
    mutex_.unlock();
    return result;
  }

  int get2() {
    int result;
    mutex_.lock();
    result = suspiciously_written;
    mutex_.unlock();
    return result;
  }

  int get3() { return not_guarded; }

  int get4() { return suspiciously_read; }

  int get5() { return get_private_suspiciously_read(); }

  void write_array_under_lock_ok(char* arr1) {
    mutex_.lock();
    arr1[2] = 'c';
    mutex_.unlock();
  }

  int read_array_outside_lock_ok(char* arr2, int i) { return arr2[i]; }

 private:
  int well_guarded;
  int suspiciously_read;
  int suspiciously_written;
  int not_guarded;
  std::mutex mutex_;

  int get_private_suspiciously_read() { return suspiciously_read; }
};
}
