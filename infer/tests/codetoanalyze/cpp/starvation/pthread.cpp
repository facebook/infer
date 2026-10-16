/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <pthread.h>
#include <mutex>
#include "recursive_mutex.h"

// mock of a header-only recursive mutex implemented with pthreads
namespace boost {
class recursive_mutex {
 public:
  recursive_mutex() {
    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(&m_, &attr);
    pthread_mutexattr_destroy(&attr);
  }
  void lock() { pthread_mutex_lock(&m_); }
  void unlock() { pthread_mutex_unlock(&m_); }

 private:
  pthread_mutex_t m_;
};
} // namespace boost

namespace pthread {

class Pthread {
 public:
  void lock_twice_bad() {
    pthread_mutex_lock(&mutex_);
    pthread_mutex_lock(&mutex_);
    x_++;
    pthread_mutex_unlock(&mutex_);
    pthread_mutex_unlock(&mutex_);
  }

  void increment() {
    pthread_mutex_lock(&mutex_);
    x_++;
    pthread_mutex_unlock(&mutex_);
  }

  void call_under_lock_bad() {
    pthread_mutex_lock(&mutex_);
    increment();
    pthread_mutex_unlock(&mutex_);
  }

  void lock_sequentially_ok() {
    increment();
    increment();
  }

 private:
  pthread_mutex_t mutex_ = PTHREAD_MUTEX_INITIALIZER;
  int x_;
};

// a mutex class that is not modelled, implemented with pthreads
class InlineMutex {
 public:
  void lock() { pthread_mutex_lock(&mutex_); }
  void unlock() { pthread_mutex_unlock(&mutex_); }

 private:
  pthread_mutex_t mutex_ = PTHREAD_MUTEX_INITIALIZER;
};

class UseInlineMutex {
 public:
  void increment() {
    mutex_.lock();
    x_++;
    mutex_.unlock();
  }

  void call_under_lock_bad() {
    mutex_.lock();
    increment();
    mutex_.unlock();
  }

 private:
  InlineMutex mutex_;
  int x_;
};

class BoostRecursiveMutex {
 public:
  void increment() {
    mutex_.lock();
    x_++;
    mutex_.unlock();
  }

  void call_under_lock_ok() {
    mutex_.lock();
    increment();
    mutex_.unlock();
  }

  void lock_guard_twice_ok() {
    std::lock_guard<boost::recursive_mutex> l1(mutex_);
    std::lock_guard<boost::recursive_mutex> l2(mutex_);
  }

 private:
  boost::recursive_mutex mutex_;
  int x_;
};

class UseRecursiveMutex {
 public:
  void increment() {
    mutex_.lock();
    x_++;
    mutex_.unlock();
  }

  void call_under_lock_ok() {
    mutex_.lock();
    increment();
    mutex_.unlock();
  }

 private:
  RecursiveMutex mutex_;
  int x_;
};

#ifdef PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#define RECURSIVE_MUTEX_INITIALIZER PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#else
#define RECURSIVE_MUTEX_INITIALIZER PTHREAD_RECURSIVE_MUTEX_INITIALIZER
#endif

class RecursiveInitializer {
 public:
  void lock_twice_ok() {
    pthread_mutex_lock(&mutex_);
    pthread_mutex_lock(&mutex_);
    pthread_mutex_unlock(&mutex_);
    pthread_mutex_unlock(&mutex_);
  }

 private:
  pthread_mutex_t mutex_ = RECURSIVE_MUTEX_INITIALIZER;
};

void use_recursive_initializer() {
  RecursiveInitializer r;
  r.lock_twice_ok();
}

// the in-class initializer is only seen in the implicit constructor, which is
// not emitted here
class NotConstructedRecursiveInitializer {
 public:
  void FP_lock_twice_ok() {
    pthread_mutex_lock(&mutex_);
    pthread_mutex_lock(&mutex_);
    pthread_mutex_unlock(&mutex_);
    pthread_mutex_unlock(&mutex_);
  }

 private:
  pthread_mutex_t mutex_ = RECURSIVE_MUTEX_INITIALIZER;
};

class RecursiveInitializerInConstructor {
 public:
  RecursiveInitializerInConstructor() : mutex_(RECURSIVE_MUTEX_INITIALIZER) {}

  void lock_twice_ok() {
    pthread_mutex_lock(&mutex_);
    pthread_mutex_lock(&mutex_);
    pthread_mutex_unlock(&mutex_);
    pthread_mutex_unlock(&mutex_);
  }

 private:
  pthread_mutex_t mutex_;
};

} // namespace pthread
