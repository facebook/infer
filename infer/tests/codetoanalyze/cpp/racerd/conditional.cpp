/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

class Conditional {
 public:
  Conditional() {}

  int get_x() { return x; }

  bool owns() {
    // temporary is properly destroyed and lock released
    return std::unique_lock<std::mutex>(mutex_).owns_lock();
  }

  void run_ok() {
    if (owns()) {
    }

    x = 0;
  }

  int get_y() { return y; }

  void run_FP() {
    // temporary not destroyed, so lock stays acquired at store to [x]
    if (std::unique_lock<std::mutex>(mutex_).owns_lock()) {
    }

    y = 0;
  }

  void set_z(int v) {
    std::lock_guard<std::mutex> l(mutex_);
    z = v;
  }

  int get_z_after_unlock_bad() {
    bool locked = mutex_.try_lock();
    mutex_.unlock();
    if (locked) {
      return z;
    }
    return 0;
  }

  int get_z_after_unlock_in_callee_bad() {
    bool locked = mutex_.try_lock();
    unlock();
    if (locked) {
      return z;
    }
    return 0;
  }

  int get_z_after_other_unlock_ok() {
    bool locked = mutex_.try_lock();
    {
      std::lock_guard<std::mutex> l(other_mutex_);
    }
    if (locked) {
      int r = z;
      mutex_.unlock();
      return r;
    }
    return 0;
  }

  int get_z_after_deferred_guard_ok() {
    bool locked = mutex_.try_lock();
    {
      std::unique_lock<std::mutex> l(other_mutex_, std::defer_lock);
    }
    if (locked) {
      int r = z;
      mutex_.unlock();
      return r;
    }
    return 0;
  }

  int get_z_after_owns_bad() {
    if (owns()) {
    }
    return z;
  }

  int get_z_after_owns_then_unlock_bad() {
    std::unique_lock<std::mutex> l(mutex_);
    bool owned = l.owns_lock();
    l.unlock();
    if (owned) {
    }
    return z;
  }

  // guards have no identity and their lock may be counted already, so the
  // result of [owns_lock()] is forgotten on any release, even for a guard
  // constructed with [std::try_to_lock], whose lock is not counted
  int FP_get_z_after_try_to_lock_guard_ok() {
    std::unique_lock<std::mutex> l(mutex_, std::try_to_lock);
    bool owned = l.owns_lock();
    {
      std::lock_guard<std::mutex> g(other_mutex_);
    }
    if (owned) {
      return z;
    }
    return 0;
  }

 private:
  void unlock() { mutex_.unlock(); }

  int x;
  int y;
  int z;
  std::mutex mutex_;
  std::mutex other_mutex_;
};
