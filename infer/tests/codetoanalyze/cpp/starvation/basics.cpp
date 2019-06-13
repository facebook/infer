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

  // deadlock between thread1_bad() and thread2_bad()
  void thread1_bad() {
    mutex_1.lock();
    mutex_2.lock();

    mutex_2.unlock();
    mutex_1.unlock();
  }

  void thread2_bad() {
    mutex_2.lock();
    mutex_1.lock();

    mutex_1.unlock();
    mutex_2.unlock();
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class WithGuard {
 public:
  WithGuard() {}

  // deadlock between thread1_bad() and thread2_bad()
  void thread1_bad() {
    std::lock_guard<std::mutex> lock1(mutex_1);
    std::lock_guard<std::mutex> lock2(mutex_2);
  }

  void thread2_bad() {
    std::lock_guard<std::mutex> lock2(mutex_2);
    std::lock_guard<std::mutex> lock1(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class DeferredGuard {
 public:
  DeferredGuard() {}

  // NO deadlock between thread1_bad() and thread2_bad()
  void thread1_ok() {
    std::unique_lock<std::mutex> lock1(mutex_1, std::defer_lock);
    std::unique_lock<std::mutex> lock2(mutex_2);
  }

  void thread2_ok() {
    std::lock_guard<std::mutex> lock2(mutex_2);
    std::lock_guard<std::mutex> lock1(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class StdLock {
 public:
  StdLock() {}

  // no reports, std::lock magically avoids deadlocks
  void foo_ok() {
    std::lock<std::mutex>(mutex_1, mutex_2);
    mutex_1.unlock();
    mutex_2.unlock();
  }

  void bar_ok() {
    std::lock<std::mutex>(mutex_2, mutex_1);
    mutex_2.unlock();
    mutex_1.unlock();
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class SelfDeadlock {
 public:
  SelfDeadlock() {}

  void thread_bad() {
    mutex_.lock();
    mutex_.lock();
    mutex_.unlock();
    mutex_.unlock();
  }

  void interproc2_bad() { std::lock_guard<std::mutex> lock(mutex_); }

  void interproc1_bad() {
    std::lock_guard<std::mutex> lock(mutex_);
    interproc2_bad();
  }

  void foo_ok() {
    { std::lock_guard<std::mutex> lock(mutex_); }
    int i = 0;
    { std::lock_guard<std::mutex> lock(mutex_); }
  }

  void bar_ok() {
    std::unique_lock<std::mutex> lock1(mutex_, std::defer_lock);
    std::lock_guard<std::mutex> lock2(mutex_);
  }

  void complicated_bad() {
    std::unique_lock<std::mutex> lock1(mutex_, std::defer_lock);
    std::lock_guard<std::mutex> lock2(mutex_);
    lock1.lock();
  }

 private:
  std::mutex mutex_;
};

class PathSensitive {
 public:
  void FP_ok() {
    std::lock_guard<std::mutex> l(mutex_);
    bool flag = false;

    if (flag) {
      std::lock_guard<std::mutex> l(mutex_);
    }
  }

 private:
  std::mutex mutex_;
};
} // namespace basics
