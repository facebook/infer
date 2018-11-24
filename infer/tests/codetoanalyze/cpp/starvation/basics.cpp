/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace basics {

class Basic {
 public:
  Basic() {}

  void thread1() {
    mutex_1.lock();
    mutex_2.lock();

    mutex_2.unlock();
    mutex_1.unlock();
  }

  void thread2() {
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

  void thread1() {
    std::lock_guard<std::mutex> lock1(mutex_1);
    std::lock_guard<std::mutex> lock2(mutex_2);
  }

  void thread2() {
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
  void thread1() {
    std::lock<std::mutex>(mutex_1, mutex_2);
    mutex_1.unlock();
    mutex_2.unlock();
  }

  void thread2() {
    std::lock<std::mutex>(mutex_2, mutex_1);
    mutex_2.unlock();
    mutex_1.unlock();
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};
} // namespace basics
