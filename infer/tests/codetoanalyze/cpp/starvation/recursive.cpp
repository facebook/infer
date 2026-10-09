/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

struct UnknownMutex {
  UnknownMutex() {}

  void lock() {}

  void unlock() {}

  UnknownMutex(const UnknownMutex&) = delete;
  UnknownMutex& operator=(const UnknownMutex&) = delete;
};

class Recursive {
 public:
  Recursive() {}

  void multi_ok() {
    std::lock_guard<std::recursive_mutex> l(recursive_mutex_);
    { std::lock_guard<std::recursive_mutex> l(recursive_mutex_); }
  }

  void unknown_ok() {
    std::lock_guard<UnknownMutex> l(umutex_);
    { std::lock_guard<UnknownMutex> l(umutex_); }
  }

  void lock_both(std::recursive_mutex& m1, std::recursive_mutex& m2) {
    std::lock_guard<std::recursive_mutex> l1(m1);
    std::lock_guard<std::recursive_mutex> l2(m2);
  }

  void lock_both_ok() { lock_both(recursive_mutex_, recursive_mutex_); }

 private:
  std::recursive_mutex recursive_mutex_;
  UnknownMutex umutex_;
};

// .inferconfig adds methods to the std::timed_mutex model, which stays
// non-recursive
class Timed {
 public:
  Timed() {}

  void relock_bad() {
    std::lock_guard<std::timed_mutex> l1(mutex_);
    std::lock_guard<std::timed_mutex> l2(mutex_);
  }

 private:
  std::timed_mutex mutex_;
};

// array indices are not tracked, so the elements of an array of locks are
// assumed to be distinct, eg with lock striping
class Stripes {
 public:
  Stripes() {}

  void lock_two_stripes_ok(int i, int j) {
    std::lock_guard<std::mutex> l1(stripes_[i]);
    std::lock_guard<std::mutex> l2(stripes_[j]);
  }

  void FN_lock_stripe_twice_bad(int i) {
    std::lock_guard<std::mutex> l1(stripes_[i]);
    std::lock_guard<std::mutex> l2(stripes_[i]);
  }

 private:
  std::mutex stripes_[16];
};
