/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

  void set_double(int new_value) {
    mutex_.lock();
    mutex_2.lock();
    double_lock_guarded = new_value;
    mutex_2.unlock();
    single_lock_guarded = new_value;
    single_lock_suspiciously_read = new_value;
    mutex_.unlock();
  }

  int test_double_lock_1_ok() {
    int result;
    mutex_2.lock();
    result = double_lock_guarded;
    mutex_2.unlock();
    return result;
  }

  int test_double_lock_2_ok() {
    int result;
    mutex_.lock();
    result = single_lock_guarded;
    mutex_.unlock();
    return result;
  }

  int test_double_lock_bad() { return single_lock_suspiciously_read; }

 private:
  int well_guarded;
  int suspiciously_read;
  int suspiciously_written;
  int not_guarded;
  int double_lock_guarded;
  int single_lock_guarded;
  int single_lock_suspiciously_read;
  std::mutex mutex_;
  std::mutex mutex_2;

  int get_private_suspiciously_read() { return suspiciously_read; }
};
} // namespace basics
