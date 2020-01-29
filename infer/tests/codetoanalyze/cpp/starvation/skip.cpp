/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

// the deadlocks here are masked by the starvation-skip-analysis option in
// .inferconfig
namespace skipped {
class Skip {
 public:
  Skip() {}

  void skipped_ok() { private_deadlock(); }

  void not_skipped_bad() { private_deadlock(); }

 private:
  std::mutex mutex_;

  void private_deadlock() {
    std::lock_guard<std::mutex> l(mutex_);
    { std::lock_guard<std::mutex> l(mutex_); }
  }
};

template <class T>
class SkipTemplate {
 private:
  T* a_;
  std::mutex mutex_;

  void private_deadlock() {
    std::lock_guard<std::mutex> l(mutex_);
    { std::lock_guard<std::mutex> l(mutex_); }
  }

 public:
  void skipped_ok() { private_deadlock(); }

  void not_skipped_bad() { private_deadlock(); }
};

class UseTemplate {
 public:
  void foo() {
    x.skipped_ok();
    x.not_skipped_bad();
  }

 private:
  SkipTemplate<void> x;
};

} // namespace skipped
