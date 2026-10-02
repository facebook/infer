/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>
#include <thread>

namespace thread_entry {

class PrivateMethod {
 public:
  void start() { thread_ = std::thread(&PrivateMethod::run_bad, this); }

  // deadlock between configure_bad() and run_bad()
  void configure_bad() {
    std::lock_guard<std::mutex> a(mutex_a_);
    std::lock_guard<std::mutex> b(mutex_b_);
  }

 private:
  void run_bad() {
    std::lock_guard<std::mutex> b(mutex_b_);
    std::lock_guard<std::mutex> a(mutex_a_);
  }

  std::thread thread_;
  std::mutex mutex_a_;
  std::mutex mutex_b_;
};

class PrivateNotStarted {
 public:
  void configure_ok() {
    std::lock_guard<std::mutex> a(mutex_a_);
    std::lock_guard<std::mutex> b(mutex_b_);
  }

 private:
  // neither called nor started as a thread
  void helper_ok() {
    std::lock_guard<std::mutex> b(mutex_b_);
    std::lock_guard<std::mutex> a(mutex_a_);
  }

  std::mutex mutex_a_;
  std::mutex mutex_b_;
};

class Lambda {
 public:
  // the deadlock is reported in the lambda
  void start_lambda_bad() {
    thread_ = std::thread([this] {
      std::lock_guard<std::mutex> b(mutex_b_);
      std::lock_guard<std::mutex> a(mutex_a_);
    });
  }

  // the other lock order is looked for in the methods of the class, which do
  // not include the lambda
  void FN_configure_bad() {
    std::lock_guard<std::mutex> a(mutex_a_);
    std::lock_guard<std::mutex> b(mutex_b_);
  }

 private:
  std::thread thread_;
  std::mutex mutex_a_;
  std::mutex mutex_b_;
};

} // namespace thread_entry
