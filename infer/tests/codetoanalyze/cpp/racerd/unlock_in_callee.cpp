/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace unlock_in_callee {

// RAII guard that is not modelled
class Guard {
 public:
  explicit Guard(std::mutex& m) : m_(m) { m_.lock(); }
  ~Guard() { m_.unlock(); }

 private:
  std::mutex& m_;
};

// mutex wrapper that is not modelled
class Mutex {
 public:
  void lock() { m_.lock(); }
  void unlock() { m_.unlock(); }

 private:
  std::mutex m_;
};

class LockGuardHolder {
 public:
  explicit LockGuardHolder(std::mutex& m) : lock_(m) {}

 private:
  std::lock_guard<std::mutex> lock_;
};

class ScopedLockHolder {
 public:
  explicit ScopedLockHolder(std::mutex& m) : lock_(m) {}

 private:
  std::scoped_lock<std::mutex> lock_;
};

class TwoMutexScopedLockHolder {
 public:
  TwoMutexScopedLockHolder(std::mutex& m1, std::mutex& m2) : lock_(m1, m2) {}

 private:
  std::scoped_lock<std::mutex, std::mutex> lock_;
};

class UniqueLockHolder {
 public:
  UniqueLockHolder() {}

  explicit UniqueLockHolder(std::mutex& m) : lock_(m) {}

 private:
  std::unique_lock<std::mutex> lock_;
};

class UniqueLockGuard {
 public:
  explicit UniqueLockGuard(std::mutex& m) : lock_(m) {}

  ~UniqueLockGuard() {}

 private:
  std::unique_lock<std::mutex> lock_;
};

class UnlockingUniqueLockGuard {
 public:
  explicit UnlockingUniqueLockGuard(std::mutex& m) : lock_(m) {}

  ~UnlockingUniqueLockGuard() { lock_.unlock(); }

 private:
  std::unique_lock<std::mutex> lock_;
};

[[noreturn]] void fatal_error();

// never returns, but is not declared [[noreturn]]
void fail(const char*) { fatal_error(); }

class FatalMessage {
 public:
  FatalMessage& operator<<(const char*) { return *this; }

  // never returns, but is not declared [[noreturn]]
  ~FatalMessage() { fatal_error(); }
};

class Logger {
 public:
  ~Logger() { flushed_ = true; }

 private:
  bool flushed_;
};

class UnlockInCallee {
 public:
  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    x_ = v;
  }

  int get_under_guard_ok() {
    Guard g(mu_);
    return x_;
  }

  int get_after_guard_scope_bad() {
    {
      Guard g(mu2_);
    }
    return x_;
  }

  int get_after_guard_in_callee_bad() {
    use_guard();
    return x_;
  }

  int get_after_wrapper_unlock_bad() {
    wrapped_mu_.lock();
    wrapped_mu_.unlock();
    return x_;
  }

  int get_after_unlock_in_callee_bad() {
    mu_.lock();
    unlock_mu();
    return x_;
  }

  int get_after_unique_lock_unlocked_in_callee_bad() {
    std::unique_lock<std::mutex> l(mu_);
    unlock_unique_lock(l);
    return x_;
  }

  int get_after_lock_guard_member_scope_bad() {
    {
      LockGuardHolder h(mu2_);
    }
    return x_;
  }

  int get_after_scoped_lock_member_scope_bad() {
    {
      ScopedLockHolder h(mu2_);
    }
    return x_;
  }

  int get_after_unique_lock_member_scope_bad() {
    {
      UniqueLockHolder h(mu2_);
    }
    return x_;
  }

  int get_after_unique_lock_guard_scope_bad() {
    {
      UniqueLockGuard g(mu2_);
    }
    return x_;
  }

  int get_after_unique_lock_guard_scope_ok() {
    std::lock_guard<std::mutex> l(mu_);
    {
      UniqueLockGuard g(mu2_);
    }
    return x_;
  }

  int get_after_unlocking_unique_lock_guard_scope_bad() {
    {
      UnlockingUniqueLockGuard g(mu2_);
    }
    return x_;
  }

  int get_after_unlocking_unique_lock_guard_scope_ok() {
    std::lock_guard<std::mutex> l(mu_);
    {
      UnlockingUniqueLockGuard g(mu2_);
    }
    return x_;
  }

  int get_after_unique_lock_unlocked_in_lambda_bad() {
    std::unique_lock<std::mutex> l(mu_);
    [&l] { l.unlock(); }();
    return x_;
  }

  int get_after_five_unlocks_in_callee_bad() {
    std::lock(mu_, mu2_, mu3_, mu4_, mu5_);
    unlock_five();
    return x_;
  }

  int get_after_two_mutex_scoped_lock_member_scope_ok() {
    std::lock_guard<std::mutex> l(mu_);
    {
      TwoMutexScopedLockHolder h(mu2_, mu3_);
    }
    return x_;
  }

  // a std::unique_lock member is assumed to own a lock when destroyed
  int FP_get_after_non_owning_unique_lock_member_scope_ok() {
    std::lock_guard<std::mutex> l(mu_);
    {
      UniqueLockHolder h;
    }
    return x_;
  }

  int get_after_unlock_relock_ok() {
    mu_.unlock();
    mu_.lock();
    int r = x_;
    mu_.unlock();
    return r;
  }

  int get_after_relock_in_callee_ok() {
    mu_.lock();
    relock_mu();
    int r = x_;
    mu_.unlock();
    return r;
  }

  int read_in_callee_after_unlock_bad() {
    std::lock_guard<std::mutex> l(mu_);
    return unlock_read_relock();
  }

  int read_in_callee_after_relock_ok() {
    std::lock_guard<std::mutex> l(mu_);
    return unlock_relock_read();
  }

  int get_after_fatal_message_ok(int i) {
    std::lock_guard<std::mutex> l(mu_);
    if (i < 0) {
      FatalMessage() << "negative index";
      return x_;
    }
    return x_;
  }

  int get_after_fail_in_callee_ok(int i) {
    std::lock_guard<std::mutex> l(mu_);
    return checked_get(i);
  }

  int get_after_fail_with_logger_in_callee_ok(int i) {
    std::lock_guard<std::mutex> l(mu_);
    if (i < 0) {
      log_and_fail("negative index");
      return x_;
    }
    return x_;
  }

  int get_after_fail_with_guard_in_callee_ok(int i) {
    std::lock_guard<std::mutex> l(mu_);
    if (i < 0) {
      guard_and_fail("negative index");
      return x_;
    }
    return x_;
  }

  int get_after_unlock_or_fail_in_callee_bad(bool ok) {
    mu_.lock();
    unlock_or_fail(ok);
    return x_;
  }

  // the callees below release only the locks that they acquire

  int get_after_early_unlock_in_callee_ok() {
    std::lock_guard<std::mutex> l(mu_);
    unique_lock_early_unlock();
    return x_;
  }

  int get_after_deferred_lock_in_callee_ok() {
    std::lock_guard<std::mutex> l(mu_);
    unique_lock_deferred();
    return x_;
  }

  int get_after_std_lock_in_callee_ok() {
    std::lock_guard<std::mutex> l(mu_);
    std_lock_unlock_both();
    return x_;
  }

  int get_after_adopt_lock_in_callee_ok() {
    std::lock_guard<std::mutex> l(mu_);
    std_lock_adopt_both();
    return x_;
  }

  // locks have no identity, so the lock held on entry that the callee releases
  // is taken to be the last one acquired by the caller
  int FP_hand_over_hand_across_calls_ok() {
    std::lock_guard<std::mutex> l(mu_);
    unlock_mu2();
    return x_;
  }

 private:
  void use_guard() { Guard g(mu2_); }

  void unlock_mu() { mu_.unlock(); }

  void unlock_mu2() { mu2_.unlock(); }

  void unlock_five() {
    mu_.unlock();
    mu2_.unlock();
    mu3_.unlock();
    mu4_.unlock();
    mu5_.unlock();
  }

  void unlock_unique_lock(std::unique_lock<std::mutex>& l) { l.unlock(); }

  void relock_mu() {
    mu_.unlock();
    mu_.lock();
  }

  int unlock_read_relock() {
    mu_.unlock();
    int r = x_;
    mu_.lock();
    return r;
  }

  int unlock_relock_read() {
    mu_.unlock();
    mu_.lock();
    return x_;
  }

  int checked_get(int i) {
    if (i < 0) {
      fail("negative index");
      return x_;
    }
    return x_;
  }

  void log_and_fail(const char* m) {
    Logger log;
    fail(m);
  }

  void guard_and_fail(const char* m) {
    Guard g(mu2_);
    fail(m);
  }

  void unlock_or_fail(bool ok) {
    if (ok) {
      mu_.unlock();
    } else {
      Logger log;
      fail("not ok");
    }
  }

  void unique_lock_early_unlock() {
    std::unique_lock<std::mutex> l(mu2_);
    l.unlock();
  }

  void unique_lock_deferred() {
    std::unique_lock<std::mutex> l(mu2_, std::defer_lock);
  }

  void std_lock_unlock_both() {
    std::lock(mu2_, mu3_);
    mu2_.unlock();
    mu3_.unlock();
  }

  void std_lock_adopt_both() {
    std::lock(mu2_, mu3_);
    std::lock_guard<std::mutex> l2(mu2_, std::adopt_lock);
    std::lock_guard<std::mutex> l3(mu3_, std::adopt_lock);
  }

  std::mutex mu_, mu2_, mu3_, mu4_, mu5_;
  Mutex wrapped_mu_;
  int x_;
};

// x_ is written after the guard is destroyed, so not under a lock
class WriteAfterUniqueLockGuardScope {
 public:
  void write() {
    {
      UniqueLockGuard g(mu_);
    }
    x_ = 1;
  }

  int read_ok() {
    {
      std::lock_guard<std::mutex> l(mu_);
    }
    return x_;
  }

 private:
  std::mutex mu_;
  int x_;
};

// a class that uses locks only through callees is not considered concurrent
class GuardOnly {
 public:
  void set(int v) {
    Guard g(mu_);
    x_ = v;
  }

  int FN_get_after_guard_scope_bad() {
    {
      Guard g(mu_);
    }
    return x_;
  }

 private:
  std::mutex mu_;
  int x_;
};

// accesses in destructors are not reported
class ReadInDestructor {
 public:
  ~ReadInDestructor() { y_ = x_; }

  void set(int v) {
    std::lock_guard<std::mutex> l(mu_);
    x_ = v;
  }

 private:
  std::mutex mu_;
  int x_;
  int y_;
};

} // namespace unlock_in_callee
