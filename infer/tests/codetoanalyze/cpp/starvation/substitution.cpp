/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace substitution {

// locks taken in a callee on a local object are not tracked, and must not be
// confused with locks of the caller's parameters
struct Holder {
  void lock_it() { mutex_.lock(); }

  void unlock_it() { mutex_.unlock(); }

  void lock_local_and_this_ok() {
    Holder tmp;
    tmp.lock_it();
    lock_it();
    unlock_it();
    tmp.unlock_it();
  }

  std::mutex mutex_;
};

void two_locals_ok() {
  Holder h1;
  Holder h2;
  h1.lock_it();
  h2.lock_it();
  h2.unlock_it();
  h1.unlock_it();
}

// locks rooted at any parameter of the callee are substituted, even if the
// caller has fewer parameters than the callee
class ParamLocks {
 public:
  void lock_param(std::mutex* m) { m->lock(); }

  void unlock_param(std::mutex* m) { m->unlock(); }

  void thread1_bad() {
    mutex_1.lock();
    lock_param(&mutex_2);
    unlock_param(&mutex_2);
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

// locks taken by the callee of a callee are substituted at each call
class TwoLevels {
 public:
  void lock_param(std::mutex* m) { m->lock(); }

  void lock_and_unlock_param(std::mutex* m) {
    lock_param(m);
    m->unlock();
  }

  void thread1_bad() {
    mutex_1.lock();
    lock_and_unlock_param(&mutex_2);
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

// locks left held on an object returned by a call are kept
class Returned {
 public:
  static Returned* get();

  void lock_it() { mutex_.lock(); }

  void unlock_it() { mutex_.unlock(); }

  void thread1_bad() {
    get()->lock_it();
    mutex_1.lock();
    mutex_1.unlock();
    get()->unlock_it();
  }

  void thread2_bad() {
    mutex_1.lock();
    lock_it();
    unlock_it();
    mutex_1.unlock();
  }

 private:
  std::mutex mutex_;
  std::mutex mutex_1;
};

// critical sections of a callee on an object the caller cannot name, eg a fresh
// one, are not kept
class Fresh;

struct Locked {
  void lock_and_unlock() { std::lock_guard<std::mutex> l(mutex_); }

  void lock_then_ok(Fresh* f);

  std::mutex mutex_;
};

class Fresh {
 public:
  void fresh_ok() {
    std::lock_guard<std::mutex> l(mutex_);
    Locked* p = new Locked();
    p->lock_and_unlock();
    delete p;
  }

  std::mutex mutex_;
};

void Locked::lock_then_ok(Fresh* f) {
  std::lock_guard<std::mutex> l(mutex_);
  std::lock_guard<std::mutex> l2(f->mutex_);
}
} // namespace substitution
