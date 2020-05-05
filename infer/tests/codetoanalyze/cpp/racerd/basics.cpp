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

  void set_not_guarded_ok(int new_value) { not_guarded = new_value; }

  int get_not_guarded_ok() { return not_guarded; }

  void set_well_guarded_ok(int new_value) {
    mutex_.lock();
    well_guarded = new_value;
    mutex_.unlock();
  }

  int get_well_guarded_ok() {
    int result;
    mutex_.lock();
    result = well_guarded;
    mutex_.unlock();
    return result;
  }

  void set_suspiciously_read_bad(int new_value) {
    mutex_.lock();
    suspiciously_read = new_value;
    mutex_.unlock();
  }

  int get_suspiciously_read_bad() { return suspiciously_read; }

  void set_suspiciously_written_ok(int new_value) {
    mutex_.lock();
    mutex_.unlock();
    suspiciously_written = new_value;
  }

  int get_suspiciously_written_ok() {
    int result;
    mutex_.lock();
    result = suspiciously_written;
    mutex_.unlock();
    return result;
  }

  void write_array_under_lock_ok(char* arr1) {
    mutex_.lock();
    arr1[2] = 'c';
    mutex_.unlock();
  }

  int read_array_outside_lock_ok(char* arr2, int i) { return arr2[i]; }

  void set_double_lock_guarded_ok(int new_value) {
    mutex_.lock();
    mutex_2.lock();
    double_lock_guarded = new_value;
    mutex_2.unlock();
    mutex_.unlock();
  }

  int read_double_lock_guarded_ok() {
    int result;
    mutex_2.lock();
    result = double_lock_guarded;
    mutex_2.unlock();
    return result;
  }

  void set_double_suspiciously_read_bad(int new_value) {
    mutex_.lock();
    mutex_2.lock();
    single_lock_suspiciously_read = new_value;
    mutex_2.unlock();
    mutex_.unlock();
  }

  int read_double_suspiciously_read_bad() {
    return single_lock_suspiciously_read;
  }

 private:
  int well_guarded;
  int suspiciously_read;
  int suspiciously_written;
  int not_guarded;
  int double_lock_guarded;
  int single_lock_suspiciously_read;
  std::mutex mutex_;
  std::mutex mutex_2;

  int get_private_suspiciously_read_ok() { return suspiciously_read; }
};
} // namespace basics
