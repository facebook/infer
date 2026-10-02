/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

// mock of android::Mutex and its scoped guard
namespace android {
class Mutex {
 public:
  int lock();
  void unlock();
  int tryLock();

  class Autolock {
   public:
    explicit Autolock(Mutex& mutex) : mLock(mutex) { mLock.lock(); }
    explicit Autolock(Mutex* mutex) : mLock(*mutex) { mLock.lock(); }
    ~Autolock() { mLock.unlock(); }

   private:
    Mutex& mLock;
  };
};

typedef Mutex::Autolock AutoMutex;
} // namespace android

namespace android_mutex {

class WithAutolock {
 public:
  void thread1_bad() {
    android::Mutex::Autolock lock1(mutex_1);
    android::Mutex::Autolock lock2(mutex_2);
  }

  void thread2_bad() {
    android::Mutex::Autolock lock2(mutex_2);
    android::Mutex::Autolock lock1(mutex_1);
  }

 private:
  android::Mutex mutex_1;
  android::Mutex mutex_2;
};

class WithAutoMutexPointer {
 public:
  void thread1_bad() {
    android::AutoMutex lock1(&mutex_1);
    android::AutoMutex lock2(&mutex_2);
  }

  void thread2_bad() {
    android::AutoMutex lock2(&mutex_2);
    android::AutoMutex lock1(&mutex_1);
  }

 private:
  android::Mutex mutex_1;
  android::Mutex mutex_2;
};

class Direct {
 public:
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
  android::Mutex mutex_1;
  android::Mutex mutex_2;
};

class SameOrder {
 public:
  void thread1_ok() {
    android::Mutex::Autolock lock1(mutex_1);
    android::Mutex::Autolock lock2(mutex_2);
  }

  void thread2_ok() {
    android::Mutex::Autolock lock1(mutex_1);
    android::Mutex::Autolock lock2(mutex_2);
  }

 private:
  android::Mutex mutex_1;
  android::Mutex mutex_2;
};

class SequentialScopes {
 public:
  void thread1_ok() {
    {
      android::Mutex::Autolock lock1(mutex_1);
    }
    {
      android::Mutex::Autolock lock2(mutex_2);
    }
  }

  void thread2_ok() {
    {
      android::Mutex::Autolock lock2(mutex_2);
    }
    {
      android::Mutex::Autolock lock1(mutex_1);
    }
  }

 private:
  android::Mutex mutex_1;
  android::Mutex mutex_2;
};

// android::Mutex is not recursive
class SelfDeadlock {
 public:
  void relock_bad() {
    android::Mutex::Autolock lock1(mutex_);
    android::Mutex::Autolock lock2(mutex_);
  }

  void lock_mutex() { android::Mutex::Autolock lock(mutex_); }

  void interproc_bad() {
    android::Mutex::Autolock lock(mutex_);
    lock_mutex();
  }

  void lock_guard_bad() {
    std::lock_guard<android::Mutex> lock1(mutex_);
    std::lock_guard<android::Mutex> lock2(mutex_);
  }

 private:
  android::Mutex mutex_;
};
} // namespace android_mutex
