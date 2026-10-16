/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>
#include <mutex>

namespace return_aliases {

struct Shared {
  std::mutex a;
  std::mutex b;
};

class SharedPtrGuards {
 public:
  void ab_bad() {
    std::lock_guard<std::mutex> x(s_->a);
    std::lock_guard<std::mutex> y(s_->b);
  }

  void ba_bad() {
    std::lock_guard<std::mutex> y(s_->b);
    std::lock_guard<std::mutex> x(s_->a);
  }

 private:
  std::shared_ptr<Shared> s_;
};

class UniquePtrLocks {
 public:
  void ab_bad() {
    s_->a.lock();
    s_->b.lock();
    s_->b.unlock();
    s_->a.unlock();
  }

  void ba_bad() {
    s_->b.lock();
    s_->a.lock();
    s_->a.unlock();
    s_->b.unlock();
  }

 private:
  std::unique_ptr<Shared> s_;
};

class ReferenceAccessor {
 public:
  void ab_bad() {
    std::lock_guard<std::mutex> x(getA());
    std::lock_guard<std::mutex> y(b_);
  }

  void ba_bad() {
    std::lock_guard<std::mutex> y(b_);
    std::lock_guard<std::mutex> x(getA());
  }

 private:
  std::mutex& getA() { return a_; }

  std::mutex a_;
  std::mutex b_;
};

class PointerAccessor {
 public:
  void ab_bad() {
    getA()->lock();
    b_.lock();
    b_.unlock();
    getA()->unlock();
  }

  void ba_bad() {
    b_.lock();
    getA()->lock();
    getA()->unlock();
    b_.unlock();
  }

 private:
  std::mutex* getA() { return &a_; }

  std::mutex a_;
  std::mutex b_;
};

class NullOnOnePath {
 public:
  void ab_bad() {
    std::lock_guard<std::mutex> x(*getA());
    std::lock_guard<std::mutex> y(b_);
  }

  void ba_bad() {
    std::lock_guard<std::mutex> y(b_);
    std::lock_guard<std::mutex> x(*getA());
  }

 private:
  std::mutex* getA() {
    if (!ready_) {
      return nullptr;
    }
    return &a_;
  }

  bool ready_;
  std::mutex a_;
  std::mutex b_;
};

std::mutex& global_mutex() {
  static std::mutex m;
  return m;
}

class StaticLocalAccessor {
 public:
  void ab_bad() {
    std::lock_guard<std::mutex> x(global_mutex());
    std::lock_guard<std::mutex> y(b_);
  }

  // reported on ab_bad() only: for ba(), the conflicting lock order is looked
  // for in the class of the static local, std::mutex
  void ba() {
    std::lock_guard<std::mutex> y(b_);
    std::lock_guard<std::mutex> x(global_mutex());
  }

 private:
  std::mutex b_;
};

class AccessorSelfDeadlock {
 public:
  void relock_bad() {
    std::lock_guard<std::mutex> x(getA());
    a_.lock();
    a_.unlock();
  }

 private:
  std::mutex& getA() { return a_; }

  std::mutex a_;
};

struct Inner {
  void ab_bad() {
    std::lock_guard<std::mutex> x(a);
    std::lock_guard<std::mutex> y(b);
  }

  void ba_bad() {
    std::lock_guard<std::mutex> y(b);
    std::lock_guard<std::mutex> x(a);
  }

  std::mutex a;
  std::mutex b;
};

class CalleeLocksThroughUniquePtr {
 public:
  void ab_bad() { inner_->ab_bad(); }

  void ba_bad() { inner_->ba_bad(); }

 private:
  std::unique_ptr<Inner> inner_;
};

class SameOrder {
 public:
  void ab1_ok() {
    std::lock_guard<std::mutex> x(getA());
    std::lock_guard<std::mutex> y(b_);
  }

  void ab2_ok() {
    std::lock_guard<std::mutex> x(a_);
    std::lock_guard<std::mutex> y(b_);
  }

 private:
  std::mutex& getA() { return a_; }

  std::mutex a_;
  std::mutex b_;
};

class UniquePtrSelfDeadlock {
 public:
  void relock_bad() {
    std::lock_guard<std::mutex> x(s_->a);
    s_->a.lock();
    s_->a.unlock();
  }

 private:
  std::unique_ptr<Shared> s_;
};

class UniquePtrToMutex {
 public:
  void relock_bad() {
    std::lock_guard<std::mutex> x(*m_);
    std::lock_guard<std::mutex> y(*m_);
  }

 private:
  std::unique_ptr<std::mutex> m_;
};

class UniquePtrToRecursiveMutex {
 public:
  void relock_ok() {
    std::lock_guard<std::recursive_mutex> x(*m_);
    std::lock_guard<std::recursive_mutex> y(*m_);
  }

 private:
  std::unique_ptr<std::recursive_mutex> m_;
};

class LockArrayAccessor {
 public:
  // the two mutexes are distinct elements of [locks_], but array indices are
  // abstracted away
  void FP_lock_array_accessor_ok() {
    std::lock_guard<std::mutex> x(lockFor(0));
    std::lock_guard<std::mutex> y(lockFor(1));
  }

 private:
  std::mutex& lockFor(int i) { return locks_[i]; }

  std::mutex locks_[2];
};

} // namespace return_aliases
