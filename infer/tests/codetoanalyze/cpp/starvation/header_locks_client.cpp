/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "header_locks.h"

// deadlocks whose first lock is taken in a header should be reported in this
// file, at the call that takes the lock
namespace header_locks {

class Inversion {
 public:
  void ab_bad() {
    a_.lock();
    b_.lock();
    b_.unlock();
    a_.unlock();
  }

  void ba_bad() {
    b_.lock();
    a_.lock();
    a_.unlock();
    b_.unlock();
  }

 private:
  Mutex a_, b_;
};

class SelfDeadlock {
 public:
  void relock_bad() {
    m_.lock();
    m_.lock();
    m_.unlock();
    m_.unlock();
  }

  void relock_after_unlock_ok() {
    m_.lock();
    m_.unlock();
    m_.lock();
    m_.unlock();
  }

 private:
  Mutex m_;
};

class TwoLocksInCallee {
 public:
  void lock_ab_then_c_bad() {
    lock_ab();
    c_.lock();
    c_.unlock();
    b_.unlock();
    a_.unlock();
  }

  void c_then_b_bad() {
    c_.lock();
    b_.lock();
    b_.unlock();
    c_.unlock();
  }

 private:
  // returns with both locks held
  void lock_ab() {
    a_.lock();
    b_.lock();
  }

  Mutex a_, b_, c_;
};

} // namespace header_locks
