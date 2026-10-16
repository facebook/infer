/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

// clang thread safety annotations, see
// https://clang.llvm.org/docs/ThreadSafetyAnalysis.html
#define GUARDED_BY(x) __attribute__((guarded_by(x)))
#define REQUIRES(...) __attribute__((requires_capability(__VA_ARGS__)))
#define REQUIRES_SHARED(...) \
  __attribute__((requires_shared_capability(__VA_ARGS__)))
#define EXCLUSIVE_LOCKS_REQUIRED(...) \
  __attribute__((exclusive_locks_required(__VA_ARGS__)))
#define ACQUIRE(...) __attribute__((acquire_capability(__VA_ARGS__)))
#define RELEASE(...) __attribute__((release_capability(__VA_ARGS__)))
#define ASSERT_CAPABILITY(x) __attribute__((assert_capability(x)))
#define SCOPED_CAPABILITY __attribute__((scoped_lockable))
#define NO_THREAD_SAFETY_ANALYSIS __attribute__((no_thread_safety_analysis))

namespace thread_safety_annotations {

// negative requirements `REQUIRES(!mu)` need `operator!`
class __attribute__((capability("mutex"))) Mutex : public std::mutex {
 public:
  const Mutex& operator!() const { return *this; }
  void AssertHeld() const ASSERT_CAPABILITY(this);
};

class Registry {
 public:
  void add(int v) {
    std::lock_guard<std::mutex> lock(mu_);
    add_locked_ok(v);
  }

  int total() {
    std::lock_guard<std::mutex> lock(mu_);
    return total_locked_ok();
  }

  void set_ok(int v) {
    std::lock_guard<std::mutex> lock(mu_);
    total_ = v;
  }

  void add_locked_ok(int v) REQUIRES(mu_) { total_ += v; }

  int total_locked_ok() REQUIRES(mu_) { return total_; }

  int total_locked_shared_ok() REQUIRES_SHARED(mu_) { return total_; }

  int total_locked_old_spelling_ok() EXCLUSIVE_LOCKS_REQUIRED(mu_) {
    return total_;
  }

  int total_locked_out_of_line_ok() REQUIRES(mu_);

  int total_locked_not_other_ok() REQUIRES(mu_, !other_mu_) { return total_; }

  void reset_locked_ok() REQUIRES(mu_) { total_ = 0; }

  int total_unlocked_bad() { return total_; }

  int total_not_locked_bad() REQUIRES(!mu_) { return total_; }

  int total_not_locked_two_attributes_bad() REQUIRES(!mu_)
      REQUIRES(!other_mu_) {
    return total_;
  }

  int total_unlock_in_between_bad() REQUIRES(mu_) {
    mu_.unlock();
    int t = total_;
    mu_.lock();
    return t;
  }

  int total_unlock_in_between_redeclared_bad() REQUIRES(mu_);

  int total_unlock_other_in_between_ok() REQUIRES(mu_, other_mu_) {
    other_mu_.unlock();
    int t = total_;
    other_mu_.lock();
    return t;
  }

  // RacerD does not model assert_capability
  int FP_total_assert_held_ok() {
    mu_.AssertHeld();
    return total_;
  }

  // RacerD does not model no_thread_safety_analysis
  void FP_reset_no_analysis_ok() NO_THREAD_SAFETY_ANALYSIS { total_ = 0; }

  // reported with --racerd-guardedby
  void reset_unlocked_bad() { total_ = 0; }

  int call_locked_without_lock_bad() { return total_locked_ok(); }

  int call_helper_unlock_in_between_bad() {
    std::lock_guard<std::mutex> lock(mu_);
    return helper_unlock_in_between();
  }

  int total_unlock_in_between_explicit_this_bad() REQUIRES(mu_);

 private:
  int helper_unlock_in_between() REQUIRES(mu_) {
    mu_.unlock();
    int t = total_;
    mu_.lock();
    return t;
  }

  Mutex mu_;
  Mutex other_mu_;
  int total_ GUARDED_BY(mu_) = 0;
};

int Registry::total_locked_out_of_line_ok() { return total_; }

int Registry::total_unlock_in_between_redeclared_bad() REQUIRES(mu_) {
  mu_.unlock();
  int t = total_;
  mu_.lock();
  return t;
}

int Registry::total_unlock_in_between_explicit_this_bad() REQUIRES(this->mu_) {
  mu_.unlock();
  int t = total_;
  mu_.lock();
  return t;
}

class OnlyRequires {
 public:
  void set_locked(int v) REQUIRES(mu_) { x_ = v; }

  // a class is only checked if one of its methods takes a lock, here only the
  // callers of set_locked do
  int FN_get_unlocked_bad() { return x_; }

  void FN_reset_unlocked_bad() { x_ = 0; }

 private:
  Mutex mu_;
  int x_ GUARDED_BY(mu_) = 0;
};

// only the writer at the head of a queue (not modelled) uses log_, which it
// does with mu_ released
class WriterQueue {
 public:
  void FP_write_ok(int v) {
    mu_.lock();
    pending_ = v;
    mu_.unlock();
    int log = log_;
    mu_.lock();
    pending_ = log;
    mu_.unlock();
  }

  void rotate_log_locked() REQUIRES(mu_) { log_++; }

 private:
  Mutex mu_;
  int pending_ GUARDED_BY(mu_) = 0;
  int log_ = 0;
};

// a lock that RacerD does not model, eg because it is implemented in another
// translation unit
class __attribute__((capability("mutex"))) OpaqueMutex {
 public:
  void Lock() ACQUIRE();
  void Unlock() RELEASE();
};

class SCOPED_CAPABILITY OpaqueMutexLock {
 public:
  explicit OpaqueMutexLock(OpaqueMutex* mu) ACQUIRE(mu) : mu_(mu) {
    mu_->Lock();
  }
  ~OpaqueMutexLock() RELEASE() { mu_->Unlock(); }

 private:
  OpaqueMutex* mu_;
};

class OpaqueLock {
 public:
  void set_ok(int v) {
    OpaqueMutexLock lock(&mu_);
    x_ = v;
  }

  int get_ok() {
    OpaqueMutexLock lock(&mu_);
    return x_;
  }

  void increment_locked_ok() REQUIRES(mu_) { x_++; }

 private:
  OpaqueMutex mu_;
  int x_ GUARDED_BY(mu_) = 0;
};

template <typename T>
class Template {
 public:
  void set(T v) {
    std::lock_guard<std::mutex> lock(mu_);
    x_ = v;
  }

  T get() {
    std::lock_guard<std::mutex> lock(mu_);
    return get_locked_ok();
  }

  T get_locked_ok() REQUIRES(mu_) { return x_; }

 private:
  Mutex mu_;
  T x_;
};

void instantiate_template(Template<int>& t) {
  t.set(1);
  t.get();
}

struct Edit {
  void set_seq(int s) { seq_ = s; }
  int encode() const { return seq_; }
  int seq_ = 0;
};

// the caller owns the edit, and the protocol (not modelled) allows one caller
// at a time; also reported when the method takes mu_ itself instead of
// requiring it
class Versions {
 public:
  void FP_log_and_apply_ok(Edit* edit) REQUIRES(mu_) {
    edit->set_seq(last_seq_);
    mu_.unlock();
    int record = edit->encode();
    mu_.lock();
    last_record_ = record;
  }

 private:
  Mutex mu_;
  int last_seq_ GUARDED_BY(mu_) = 0;
  int last_record_ GUARDED_BY(mu_) = 0;
};

} // namespace thread_safety_annotations
