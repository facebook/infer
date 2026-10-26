/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

// a callee may release a lock held by its caller and take it again later
namespace release_in_callee {

void sleep_a_bit();

class Mutex {
 public:
  void Lock() { mu_.lock(); }

  void Unlock() { mu_.unlock(); }

 private:
  std::mutex mu_;
};

class MutexLock {
 public:
  explicit MutexLock(Mutex* mu) : mu_(mu) { mu_->Lock(); }

  ~MutexLock() { mu_->Unlock(); }

 private:
  Mutex* const mu_;
};

class Relock {
 public:
  void guard_ok() {
    MutexLock l(&mutex_);
    make_room();
  }

  void lock_guard_ok() {
    std::lock_guard<std::mutex> l(mutex_1);
    relock_1();
  }

 private:
  void make_room() {
    if (busy_) {
      mutex_.Unlock();
      sleep_a_bit();
      mutex_.Lock();
    }
  }

  void relock_1() {
    if (busy_) {
      mutex_1.unlock();
      sleep_a_bit();
      mutex_1.lock();
    }
  }

  Mutex mutex_;
  std::mutex mutex_1;
  bool busy_;
};

class TakeWhileReleased {
 public:
  void thread1_ok() {
    std::lock_guard<std::mutex> l(mutex_1);
    take_2();
  }

  void thread2_ok() {
    std::lock_guard<std::mutex> lock2(mutex_2);
    std::lock_guard<std::mutex> lock1(mutex_1);
  }

 private:
  void take_2() {
    if (busy_) {
      mutex_1.unlock();
      {
        std::lock_guard<std::mutex> lock2(mutex_2);
      }
      mutex_1.lock();
    }
  }

  std::mutex mutex_1;
  std::mutex mutex_2;
  bool busy_;
};

class RelockUnderOtherLock {
 public:
  // retakes mutex_1 while holding mutex_2
  void relock_bad() {
    std::lock_guard<std::mutex> lock1(mutex_1);
    std::lock_guard<std::mutex> lock2(mutex_2);
    relock_1();
  }

 private:
  void relock_1() {
    if (busy_) {
      mutex_1.unlock();
      sleep_a_bit();
      mutex_1.lock();
    }
  }

  std::mutex mutex_1;
  std::mutex mutex_2;
  bool busy_;
};
} // namespace release_in_callee
