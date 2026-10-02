/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace const_methods {

class ConstMethods {
 public:
  // deadlock between const_thread1_bad() and thread2_bad()
  void const_thread1_bad() const {
    std::lock_guard<std::mutex> lock1(mutex_1);
    std::lock_guard<std::mutex> lock2(mutex_2);
  }

  void thread2_bad() {
    std::lock_guard<std::mutex> lock2(mutex_2);
    std::lock_guard<std::mutex> lock1(mutex_1);
  }

  // no deadlock: both threads hold gate_ first
  void gated_const_thread1_ok() const {
    std::lock_guard<std::mutex> gate(gate_);
    std::lock_guard<std::mutex> lock3(mutex_3);
    std::lock_guard<std::mutex> lock4(mutex_4);
  }

  void gated_thread2_ok() {
    std::lock_guard<std::mutex> gate(gate_);
    std::lock_guard<std::mutex> lock4(mutex_4);
    std::lock_guard<std::mutex> lock3(mutex_3);
  }

  // x.FN_copy_from_bad(&y) and y.FN_copy_to_bad(&x) deadlock, but across
  // threads the mutex_1 of any two objects of the class is the same lock
  void FN_copy_from_bad(const ConstMethods* src) {
    std::lock_guard<std::mutex> lock1(mutex_1);
    std::lock_guard<std::mutex> src_lock1(src->mutex_1);
  }

  void FN_copy_to_bad(ConstMethods* dst) const {
    std::lock_guard<std::mutex> lock1(mutex_1);
    std::lock_guard<std::mutex> dst_lock1(dst->mutex_1);
  }

 private:
  mutable std::mutex mutex_1;
  mutable std::mutex mutex_2;
  mutable std::mutex mutex_3;
  mutable std::mutex mutex_4;
  mutable std::mutex gate_;
};

class ConstParams {
 public:
  // deadlock between lock_const_param_bad() and lock_param_bad()
  void lock_const_param_bad(const ConstParams* other) {
    std::lock_guard<std::mutex> lock1(other->mutex_1);
    std::lock_guard<std::mutex> lock2(other->mutex_2);
  }

  void lock_param_bad(ConstParams* other) {
    std::lock_guard<std::mutex> lock2(other->mutex_2);
    std::lock_guard<std::mutex> lock1(other->mutex_1);
  }

 private:
  mutable std::mutex mutex_1;
  mutable std::mutex mutex_2;
};
} // namespace const_methods
