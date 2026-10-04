/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace lock_in_callee {

class PrivateHelper {
 public:
  void set(int v) { set_locked(v); }

  int get_bad() { return x_; }

  int get_locked_ok() { return get_locked(); }

  void set_y(int v) {
    touch();
    y_ = v;
  }

  int get_y_ok() { return y_; }

 private:
  void set_locked(int v) {
    std::lock_guard<std::mutex> l(mutex_);
    x_ = v;
  }

  int get_locked() {
    std::lock_guard<std::mutex> l(mutex_);
    return x_;
  }

  void touch() { std::lock_guard<std::mutex> l(mutex_); }

  std::mutex mutex_;
  int x_;
  int y_;
};

// mutex wrapper and RAII guard that are not modelled
class Mutex {
 public:
  void lock() { mutex_.lock(); }
  void unlock() { mutex_.unlock(); }

 private:
  std::mutex mutex_;
};

class Guard {
 public:
  explicit Guard(Mutex& m) : m_(m) { m_.lock(); }
  ~Guard() { m_.unlock(); }

 private:
  Mutex& m_;
};

class CustomGuard {
 public:
  void set(int v) {
    Guard g(mutex_);
    x_ = v;
  }

  int get_bad() { return x_; }

  void set_y(int v) {
    mutex_.lock();
    y_ = v;
    mutex_.unlock();
  }

  int get_y_bad() { return y_; }

  void set_z(int v) {
    set(v);
    z_ = v;
  }

  int get_z_ok() { return z_; }

 private:
  Mutex mutex_;
  int x_;
  int y_;
  int z_;
};

class CallsCustomGuard {
 public:
  void set(int v) {
    guarded_.set(v);
    x_ = v;
  }

  int get_ok() { return x_; }

 private:
  CustomGuard guarded_;
  int x_;
};

// a member that uses a lock does not protect the accesses of its owner
class Counter {
 public:
  void increment() {
    std::lock_guard<std::mutex> l(mutex_);
    count_++;
  }

  int get_bad() { return count_; }

 private:
  std::mutex mutex_;
  int count_;
};

class UsesCounter {
 public:
  void set(int v) {
    counter_.increment();
    x_ = v;
  }

  int get_ok() { return x_; }

  int get_count_bad() { return counter_.get_bad(); }

 private:
  Counter counter_;
  int x_;
};

// RAII guard that is not modelled, holding a std::unique_lock
class UniqueLockGuard {
 public:
  explicit UniqueLockGuard(std::mutex& m) : lock_(m) {}

  ~UniqueLockGuard() {}

 private:
  std::unique_lock<std::mutex> lock_;
};

class WriteAfterGuardScope {
 public:
  void write() {
    {
      UniqueLockGuard g(mutex_);
      x_ = 1;
    }
    y_ = 1;
  }

  int get_x_bad() { return x_; }

  int get_y_ok() { return y_; }

 private:
  std::mutex mutex_;
  int x_;
  int y_;
};

class RequiresLockHeld {
 public:
  void set(int v) {
    Guard g(mutex_);
    x_ = v;
  }

  int get() {
    Guard g(mutex_);
    return FP_get_requires_lock_held_ok();
  }

  // callers must hold [mutex_], as [get()] does
  int FP_get_requires_lock_held_ok() { return x_; }

 private:
  Mutex mutex_;
  int x_;
};

struct Slice {
  int size;
};

class FileState {
 public:
  void read(Slice* result) {
    std::lock_guard<std::mutex> l(mutex_);
    result->size = size_;
  }

 private:
  std::mutex mutex_;
  int size_;
};

// [*result] is written under the lock of the callee, but is not shared
class SequentialFile {
 public:
  int FP_read_out_parameter_ok(Slice* result) {
    file_->read(result);
    return result->size;
  }

 private:
  FileState* file_;
};

// locks have no identity, so the lock held on entry that the callee releases
// is taken to be the last one acquired by the caller
class HandOverHandWrapped {
 public:
  void set(int v) {
    Guard g(mutex_);
    x_ = v;
  }

  int FP_hand_over_hand_across_calls_ok() {
    Guard g(mutex_);
    unlock_other();
    return x_;
  }

 private:
  void unlock_other() { other_mutex_.unlock(); }

  Mutex mutex_;
  Mutex other_mutex_;
  int x_;
};

} // namespace lock_in_callee
