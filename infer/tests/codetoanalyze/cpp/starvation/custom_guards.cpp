/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <pthread.h>
#include <mutex>

// scoped guards that are not modelled are recognised from the summaries of
// their constructors and destructors
namespace custom_guards {

class RefGuard {
 public:
  explicit RefGuard(std::mutex& m) : m_(m) { m_.lock(); }

  ~RefGuard() { m_.unlock(); }

 private:
  std::mutex& m_;
};

class PtrGuard {
 public:
  explicit PtrGuard(std::mutex* m) : m_(m) { m_->lock(); }

  ~PtrGuard() { m_->unlock(); }

 private:
  std::mutex* m_;
};

// pthread mutexes are treated as recursive
class PthreadGuard {
 public:
  explicit PthreadGuard(pthread_mutex_t* m) : m_(m) { pthread_mutex_lock(m_); }

  ~PthreadGuard() { pthread_mutex_unlock(m_); }

 private:
  pthread_mutex_t* m_;
};

// a lock class that is not modelled, with a nested guard
class Mutex {
 public:
  void lock() { m_.lock(); }

  void unlock() { m_.unlock(); }

  class Guard {
   public:
    explicit Guard(Mutex& m) : m_(m) { m_.lock(); }

    ~Guard() { m_.unlock(); }

   private:
    Mutex& m_;
  };

 private:
  std::mutex m_;
};

// a guard that can release and reacquire its lock
class RelockGuard {
 public:
  explicit RelockGuard(std::mutex& m) : m_(m) { m_.lock(); }

  ~RelockGuard() { m_.unlock(); }

  void lock() { m_.lock(); }

  void unlock() { m_.unlock(); }

 private:
  std::mutex& m_;
};

// a guard whose destructor releases its lock only if it holds it
class UniqueGuard {
 public:
  explicit UniqueGuard(std::mutex& m) : m_(m), owns_(true) { m_.lock(); }

  ~UniqueGuard() {
    if (owns_) {
      m_.unlock();
    }
  }

  void lock() {
    m_.lock();
    owns_ = true;
  }

  void unlock() {
    m_.unlock();
    owns_ = false;
  }

 private:
  std::mutex& m_;
  bool owns_;
};

// releases a lock for its lifetime
class Unlocker {
 public:
  explicit Unlocker(std::mutex& m) : m_(m) { m_.unlock(); }

  ~Unlocker() { m_.lock(); }

 private:
  std::mutex& m_;
};

class TwoLockGuard {
 public:
  TwoLockGuard(std::mutex& m1, std::mutex& m2) : m1_(m1), m2_(m2) {
    m1_.lock();
    m2_.lock();
  }

  ~TwoLockGuard() {
    m2_.unlock();
    m1_.unlock();
  }

 private:
  std::mutex& m1_;
  std::mutex& m2_;
};

class HelperGuard {
 public:
  explicit HelperGuard(std::mutex& m) : m_(m) { acquire(); }

  ~HelperGuard() { m_.unlock(); }

 private:
  void acquire() { m_.lock(); }

  std::mutex& m_;
};

// releases a lock taken by the function that returns it
class Releaser {
 public:
  explicit Releaser(std::mutex* m) : m_(m) {}

  ~Releaser() { m_->unlock(); }

 private:
  std::mutex* m_;
};

// values that do not release a lock when destroyed
struct Name {
  ~Name() {}

  int id;
};

struct Point {
  int x;
  int y;
};

void trace_mutex(std::mutex* m);

// passes its mutex to a call before locking it
class TracingGuard {
 public:
  explicit TracingGuard(std::mutex* m) : m_(m) {
    trace_mutex(m_);
    m_->lock();
  }

  ~TracingGuard() { m_->unlock(); }

 private:
  std::mutex* m_;
};

// locks the mutex stored by a callee of its constructor
class RebindingGuard {
 public:
  RebindingGuard(std::mutex* initial, std::mutex* m) : m_(initial) {
    rebind(m);
    m_->lock();
  }

  ~RebindingGuard() { m_->unlock(); }

 private:
  void rebind(std::mutex* m) { m_ = m; }

  std::mutex* m_;
};

class ReassigningGuard {
 public:
  ReassigningGuard(std::mutex* initial, std::mutex* m) : m_(initial) {
    m_ = m;
    m_->lock();
  }

  ~ReassigningGuard() { m_->unlock(); }

 private:
  std::mutex* m_;
};

// leaves the release of the lock taken by its constructor to its user
class Acquirer {
 public:
  explicit Acquirer(std::mutex& m) : m_(m) { m_.lock(); }

  ~Acquirer() {}

  std::mutex& m_;
};

void release(Acquirer& acquirer) { acquirer.m_.unlock(); }

class WithRefGuard {
 public:
  void thread1_bad() {
    RefGuard lock1(mutex_1);
    RefGuard lock2(mutex_2);
  }

  void thread2_bad() {
    RefGuard lock2(mutex_2);
    RefGuard lock1(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class WithPtrGuard {
 public:
  void thread1_bad() {
    PtrGuard lock1(&mutex_1);
    PtrGuard lock2(&mutex_2);
  }

  void thread2_bad() {
    PtrGuard lock2(&mutex_2);
    PtrGuard lock1(&mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class WithPthreadGuard {
 public:
  void thread1_bad() {
    PthreadGuard lock1(&mutex_1);
    PthreadGuard lock2(&mutex_2);
  }

  void thread2_bad() {
    PthreadGuard lock2(&mutex_2);
    PthreadGuard lock1(&mutex_1);
  }

 private:
  pthread_mutex_t mutex_1;
  pthread_mutex_t mutex_2;
};

class WithNestedGuard {
 public:
  void thread1_bad() {
    Mutex::Guard lock1(mutex_1);
    Mutex::Guard lock2(mutex_2);
  }

  void thread2_bad() {
    Mutex::Guard lock2(mutex_2);
    Mutex::Guard lock1(mutex_1);
  }

 private:
  Mutex mutex_1;
  Mutex mutex_2;
};

class Interprocedural {
 public:
  void lock_2() { RefGuard lock2(mutex_2); }

  void thread1_bad() {
    RefGuard lock1(mutex_1);
    lock_2();
  }

  void thread2_bad() {
    RefGuard lock2(mutex_2);
    RefGuard lock1(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class Relock {
 public:
  void thread1_bad() {
    RelockGuard lock2(mutex_2);
    lock2.unlock();
    RefGuard lock1(mutex_1);
    lock2.lock();
  }

  void thread2_bad() {
    RefGuard lock2(mutex_2);
    RefGuard lock1(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class ConditionalRelease {
 public:
  // a guard whose destructor releases its lock only on some paths is not
  // recognised
  void FN_thread1_bad() {
    UniqueGuard lock2(mutex_2);
    lock2.unlock();
    RefGuard lock1(mutex_1);
    lock2.lock();
  }

  void FN_thread2_bad() {
    RefGuard lock2(mutex_2);
    RefGuard lock1(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class NoInversion {
 public:
  void sequential_ok() {
    {
      RefGuard lock2(mutex_2);
    }
    {
      RefGuard lock1(mutex_1);
    }
  }

  void same_order_ok() {
    RefGuard lock1(mutex_1);
    RefGuard lock2(mutex_2);
  }

  void early_unlock_ok() {
    RelockGuard lock2(mutex_2);
    lock2.unlock();
    {
      RefGuard lock1(mutex_1);
    }
    lock2.lock();
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class WithUnlocker {
 public:
  // the lock released by an Unlocker is not reacquired by its destructor
  void FN_thread1_bad() {
    RefGuard lock2(mutex_2);
    {
      Unlocker unlock2(mutex_2);
    }
    RefGuard lock1(mutex_1);
  }

  void FN_thread2_bad() {
    RefGuard lock1(mutex_1);
    RefGuard lock2(mutex_2);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class SelfDeadlock {
 public:
  void relock_bad() {
    RefGuard lock1(mutex_);
    RefGuard lock2(mutex_);
  }

 private:
  std::mutex mutex_;
};

class ReturnedGuard {
 public:
  Releaser lock_1() {
    mutex_1.lock();
    return Releaser(&mutex_1);
  }

  void thread1_bad() {
    const Releaser& lock1 = lock_1();
    RefGuard lock2(mutex_2);
  }

  void thread2_bad() {
    RefGuard lock2(mutex_2);
    RefGuard lock1(mutex_1);
  }

  void sequential_ok(bool b) {
    if (b) {
      {
        const Releaser& lock1 = lock_1();
      }
      RefGuard lock2(mutex_2);
    }
  }

  // relies on C++17's guaranteed copy elision: in C++11 the temporary that the
  // copy-initialisation is made from releases the lock at the end of the
  // full-expression, so no guard is recognised
  void copy_init_bad() {
    Releaser lock1 = lock_1();
    RefGuard lock2(mutex_2);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

// a value returned with a lock held is a guard only if its destructor releases
// a lock
class ReturnedValue {
 public:
  Name lock_1_name() {
    mutex_1.lock();
    return Name();
  }

  Point lock_1_point() {
    mutex_1.lock();
    return Point{1, 2};
  }

  Point lock_1_3_point() {
    mutex_1.lock();
    mutex_3.lock();
    return Point{1, 3};
  }

  void destroyed_name_bad() {
    {
      Name name = lock_1_name();
    }
    mutex_2.lock();
    mutex_2.unlock();
    mutex_1.unlock();
  }

  void temporary_name_bad() {
    lock_1_name();
    mutex_2.lock();
    mutex_2.unlock();
    mutex_1.unlock();
  }

  void wrap_lock_1() { Point point = lock_1_point(); }

  void wrapped_point_bad() {
    wrap_lock_1();
    mutex_2.lock();
    mutex_2.unlock();
    mutex_1.unlock();
  }

  void two_lock_point_bad() {
    Point point = lock_1_3_point();
    mutex_2.lock();
    mutex_2.unlock();
    mutex_3.unlock();
    mutex_1.unlock();
  }

  void reverse_bad() {
    mutex_2.lock();
    mutex_1.lock();
    mutex_1.unlock();
    mutex_3.lock();
    mutex_3.unlock();
    mutex_2.unlock();
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
  std::mutex mutex_3;
};

// the locks of guards that hold several locks, or that are on the heap, are
// not tracked
class Untracked {
 public:
  void two_lock_guard_ok(bool b) {
    if (b) {
      {
        TwoLockGuard lock12(mutex_1, mutex_2);
      }
      RefGuard lock3(mutex_3);
    }
  }

  void heap_guard_ok(bool b) {
    if (b) {
      RefGuard* lock1 = new RefGuard(mutex_1);
      delete lock1;
      RefGuard lock3(mutex_3);
    }
  }

  void reverse_ok() {
    RefGuard lock3(mutex_3);
    RefGuard lock1(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
  std::mutex mutex_3;
};

class WithHelperGuard {
 public:
  // the lock taken by a helper of the constructor is not recognised as held
  // by the guard
  void FN_thread1_bad() {
    HelperGuard lock1(mutex_1);
    HelperGuard lock2(mutex_2);
  }

  void FN_thread2_bad() {
    HelperGuard lock2(mutex_2);
    HelperGuard lock1(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

// an object whose destructor does not release the lock taken by its
// constructor is not a guard
class WithAcquirer {
 public:
  void guard_then_lock_bad() {
    RefGuard lock1(mutex_1);
    RefGuard lock2(mutex_2);
  }

  void acquirer_then_lock_ok() {
    Acquirer acquirer(mutex_1);
    release(acquirer);
    RefGuard lock2(mutex_2);
  }

  // the lock taken by the constructor of an object that is not a guard is
  // treated as released at once
  void FN_acquirer_held_bad() {
    Acquirer acquirer(mutex_1);
    RefGuard lock2(mutex_2);
    release(acquirer);
  }

  void reverse_bad() {
    RefGuard lock2(mutex_2);
    RefGuard lock1(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

class WithTracingGuard {
 public:
  void thread1_bad() {
    TracingGuard lock1(&mutex_1);
    TracingGuard lock2(&mutex_2);
  }

  void thread2_bad() {
    TracingGuard lock2(&mutex_2);
    TracingGuard lock1(&mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
};

// stores through a copy of `this`
class SelfAliasGuard {
 public:
  SelfAliasGuard(std::mutex* initial, std::mutex* m) {
    m_ = initial;
    SelfAliasGuard* self = this;
    self->m_ = m;
    m_->lock();
  }

  ~SelfAliasGuard() { m_->unlock(); }

 private:
  std::mutex* m_;
};

class WithRebindingGuard {
 public:
  // stores made by callees of the constructor are not tracked, so a call that
  // may write the guard's fields (any call taking `this`) before the lock makes
  // the lock unrecognised
  void FN_rebound_bad() {
    RebindingGuard lock2(&mutex_1, &mutex_2);
    std::lock_guard<std::mutex> lock3(mutex_3);
  }

  void reassigned_bad() {
    ReassigningGuard lock2(&mutex_1, &mutex_2);
    std::lock_guard<std::mutex> lock3(mutex_3);
  }

  // the store through `self` makes the lock unrecognised
  void FN_self_alias_bad() {
    SelfAliasGuard lock2(&mutex_1, &mutex_2);
    std::lock_guard<std::mutex> lock3(mutex_3);
  }

  void reverse_3_2_bad() {
    std::lock_guard<std::mutex> lock3(mutex_3);
    std::lock_guard<std::mutex> lock2(mutex_2);
  }

  void reverse_3_1_ok() {
    std::lock_guard<std::mutex> lock3(mutex_3);
    std::lock_guard<std::mutex> lock1(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex mutex_2;
  std::mutex mutex_3;
};

// a lock reached through a field has the same name in every method, even after
// a pointer is stored into the field
class StoreThenLock {
 public:
  void set_and_lock_bad(std::mutex* m) {
    other_ = m;
    std::lock_guard<std::mutex> lock1(mutex_1);
    std::lock_guard<std::mutex> lock2(*other_);
  }

  void lock_bad() {
    std::lock_guard<std::mutex> lock2(*other_);
    std::lock_guard<std::mutex> lock1(mutex_1);
  }

 private:
  std::mutex mutex_1;
  std::mutex* other_;
};

// takes and releases a lock through a field in different methods
class Session {
 public:
  void begin(std::mutex* m) {
    mutex_ = m;
    mutex_->lock();
  }

  void end() { mutex_->unlock(); }

 private:
  std::mutex* mutex_;
};

class WithSession {
 public:
  void thread1_bad() {
    session_.begin(&mutex_1);
    session_.end();
    std::lock_guard<std::mutex> lock2(mutex_2);
    std::lock_guard<std::mutex> lock3(mutex_3);
  }

  void thread2_bad() {
    std::lock_guard<std::mutex> lock3(mutex_3);
    std::lock_guard<std::mutex> lock2(mutex_2);
  }

 private:
  Session session_;
  std::mutex mutex_1;
  std::mutex mutex_2;
  std::mutex mutex_3;
};
} // namespace custom_guards
