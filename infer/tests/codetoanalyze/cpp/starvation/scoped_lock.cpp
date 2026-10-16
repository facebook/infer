/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace scoped_lock {

class OneMutex {
 public:
  void thread1_bad() {
    std::scoped_lock l(mutex_1);
    std::lock_guard<std::mutex> g(mutex_2);
  }

  void thread2_bad() {
    std::lock_guard<std::mutex> g(mutex_2);
    std::scoped_lock l(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class TwoMutexes {
 public:
  void thread1_bad() {
    std::scoped_lock l(mutex_1, mutex_2);
    std::lock_guard<std::mutex> g(mutex_3);
  }

  // reported once per mutex of the guard, when reports are not deduplicated
  void thread2_bad() {
    std::lock_guard<std::mutex> g(mutex_3);
    std::scoped_lock l(mutex_1, mutex_2);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
  std::mutex mutex_3;
};

class ThreeMutexes {
 public:
  void thread1_bad() {
    std::scoped_lock l(mutex_1, mutex_2, mutex_3);
    std::lock_guard<std::mutex> g(mutex_4);
  }

  void thread2_bad() {
    std::lock_guard<std::mutex> g(mutex_4);
    std::scoped_lock l(mutex_1, mutex_2, mutex_3);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
  std::mutex mutex_3;
  std::mutex mutex_4;
};

class ReverseOrder {
 public:
  // no reports, like std::lock, std::scoped_lock avoids deadlocks between its
  // own mutexes
  void thread1_ok() { std::scoped_lock l(mutex_1, mutex_2); }

  void thread2_ok() { std::scoped_lock l(mutex_2, mutex_1); }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class ReleasedAtEndOfScope {
 public:
  void thread1_ok() {
    {
      std::scoped_lock l(mutex_1, mutex_2);
    }
    std::lock_guard<std::mutex> g(mutex_3);
  }

  void thread2_ok() {
    std::lock_guard<std::mutex> g(mutex_3);
    std::scoped_lock l(mutex_1, mutex_2);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
  std::mutex mutex_3;
};

class SelfDeadlock {
 public:
  void relock_bad() {
    std::lock_guard<std::mutex> g(mutex_2);
    std::scoped_lock l(mutex_1, mutex_2);
  }

  void interproc_bad() {
    std::lock_guard<std::mutex> g(mutex_2);
    lock_both();
  }

  void sequential_ok() {
    {
      std::scoped_lock l(mutex_1, mutex_2);
    }
    std::scoped_lock l(mutex_1, mutex_2);
  }

 private:
  void lock_both() { std::scoped_lock l(mutex_1, mutex_2); }

  std::mutex mutex_1;
  std::mutex mutex_2;
};

class RelockNonRecursive {
 public:
  void relock_bad() {
    std::lock_guard<std::mutex> g(mutex_);
    std::scoped_lock l(recursive_mutex_, mutex_);
  }

 private:
  std::recursive_mutex recursive_mutex_;
  std::mutex mutex_;
};

class RelockRecursive {
 public:
  void relock_ok() {
    std::lock_guard<std::recursive_mutex> g(recursive_mutex_);
    std::scoped_lock l(recursive_mutex_, mutex_);
  }

 private:
  std::recursive_mutex recursive_mutex_;
  std::mutex mutex_;
};

class AdoptLock {
 public:
  // with std::adopt_lock, the mutexes are already held and the guard only
  // releases them
  void thread1_bad() {
    std::lock(mutex_1, mutex_2);
    std::scoped_lock l(std::adopt_lock, mutex_1, mutex_2);
    std::lock_guard<std::mutex> g(mutex_3);
  }

  void thread2_bad() {
    std::lock_guard<std::mutex> g(mutex_3);
    mutex_1.lock();
    std::scoped_lock l(std::adopt_lock, mutex_1);
  }

  void released_at_end_of_scope_ok() {
    {
      std::lock(mutex_1, mutex_2);
      std::scoped_lock l(std::adopt_lock, mutex_1, mutex_2);
    }
    std::lock_guard<std::mutex> g(mutex_3);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
  std::mutex mutex_3;
};

class TryLockThenAdopt {
 public:
  // trylocks are ignored and std::try_lock is not modelled, so the mutexes
  // adopted by the guard are not known to be held
  void FN_thread1_bad() {
    if (std::try_lock(mutex_1, mutex_2) == -1) {
      std::scoped_lock l(std::adopt_lock, mutex_1, mutex_2);
      std::lock_guard<std::mutex> g(mutex_3);
    }
  }

  void FN_thread2_bad() {
    std::lock_guard<std::mutex> g(mutex_3);
    std::lock_guard<std::mutex> g1(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
  std::mutex mutex_3;
};

} // namespace scoped_lock
