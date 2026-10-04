/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// mock of android::Mutex and its scoped guard
namespace android {
class Mutex {
 public:
  int lock();
  void unlock();
  // return 0 on success
  int tryLock();
  int timedLock(long long timeoutNs);

  class Autolock {
   public:
    explicit Autolock(Mutex& mutex) : mLock(mutex) { mLock.lock(); }
    ~Autolock() { mLock.unlock(); }

   private:
    Mutex& mLock;
  };
};
} // namespace android

namespace android_mutex {

class Counter {
 public:
  void increment() {
    android::Mutex::Autolock lock(mLock);
    count_++;
  }

  int get_count_bad() { return count_; }

  int get_count_ok() {
    android::Mutex::Autolock lock(mLock);
    return count_;
  }

  void try_increment_ok() {
    if (mLock.tryLock() == 0) {
      count_++;
      mLock.unlock();
    }
  }

  void timed_increment_ok() {
    if (mLock.timedLock(1000) != 0) {
      return;
    }
    count_++;
    mLock.unlock();
  }

  void failed_try_increment_bad() {
    if (mLock.tryLock() != 0) {
      count_++;
    } else {
      mLock.unlock();
    }
  }

  void stored_try_increment_ok() {
    const bool locked = mLock.tryLock() == 0;
    if (locked) {
      count_++;
      mLock.unlock();
    }
  }

 private:
  android::Mutex mLock;
  int count_;
};
} // namespace android_mutex

namespace android {
enum { OK = 0, NO_ERROR = OK };
} // namespace android

namespace android_mutex {

class StatusCounter {
 public:
  void increment() {
    android::Mutex::Autolock lock(mLock);
    count_++;
  }

  void no_error_try_increment_ok() {
    if (mLock.tryLock() == android::NO_ERROR) {
      count_++;
      mLock.unlock();
    }
  }

  void ok_timed_increment_ok() {
    if (mLock.timedLock(1000) != android::OK) {
      return;
    }
    count_++;
    mLock.unlock();
  }

 private:
  android::Mutex mLock;
  int count_;
};
} // namespace android_mutex
