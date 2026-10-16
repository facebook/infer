/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <pthread.h>
#include <future>
#include <mutex>
#include <thread>
#include <vector>

namespace thread_entry {

class PrivateMethod {
 public:
  void start() { thread_ = std::thread(&PrivateMethod::run_bad, this); }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  void run_bad() { sink_ = value_; }

  // not started as a thread: reported through its callers, if any
  void helper_ok() { sink_ = value_; }

  std::thread thread_;
  std::mutex mutex_;
  int value_;
  int sink_;
};

class PrivateMethodWithArgs {
 public:
  void start() {
    thread_ = std::thread(&PrivateMethodWithArgs::run_bad, this, 42);
  }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  void run_bad(int n) { sink_ = value_ + n; }

  std::thread thread_;
  std::mutex mutex_;
  int value_;
  int sink_;
};

class StartedByHelper {
 public:
  void start() { launch(); }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  void launch() { thread_ = std::thread(&StartedByHelper::run_bad, this); }

  void run_bad() { sink_ = value_; }

  std::thread thread_;
  std::mutex mutex_;
  int value_;
  int sink_;
};

class Async {
 public:
  void start() {
    future_ = std::async(std::launch::async, &Async::run_bad, this);
  }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  void run_bad() { sink_ = value_; }

  std::future<void> future_;
  std::mutex mutex_;
  int value_;
  int sink_;
};

class AsyncDefaultPolicy {
 public:
  void start() { future_ = std::async(&AsyncDefaultPolicy::run_bad, this); }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  void run_bad() { sink_ = value_; }

  std::future<void> future_;
  std::mutex mutex_;
  int value_;
  int sink_;
};

struct Shared {
  int value;
  int sink;
};

Shared shared;

class PthreadStaticMethod {
 public:
  void start() {
    pthread_create(&thread_, nullptr, &PthreadStaticMethod::run_bad, nullptr);
  }

  void start_other() {
    pthread_create(
        &thread_, nullptr, PthreadStaticMethod::run_other_bad, nullptr);
  }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    shared.value = v;
  }

 private:
  static void* run_bad(void*) {
    shared.sink = shared.value;
    return nullptr;
  }

  static void* run_other_bad(void*) {
    shared.sink = shared.value;
    return nullptr;
  }

  pthread_t thread_;
  std::mutex mutex_;
};

class Lambda {
 public:
  // the race is reported in the lambda
  void start_lambda_bad() {
    thread_ = std::thread([this] { sink_ = value_; });
  }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  std::thread thread_;
  std::mutex mutex_;
  int value_;
  int sink_;
};

class NamedLambda {
 public:
  // the race is reported in the lambda
  void start_named_lambda_bad() {
    auto body = [this] { sink_ = value_; };
    thread_ = std::thread(body);
  }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  std::thread thread_;
  std::mutex mutex_;
  int value_;
  int sink_;
};

class LockInEntry {
 public:
  void start() { thread_ = std::thread(&LockInEntry::run, this); }

  int get_bad() { return value_; }

 private:
  void run() {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = 1;
  }

  std::thread thread_;
  std::mutex mutex_;
  int value_;
};

class LockInCallee {
 public:
  void start() { thread_ = std::thread(&LockInCallee::run_bad, this); }

  void set(int v) { set_locked(v); }

 private:
  void set_locked(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

  void run_bad() { sink_ = value_; }

  std::thread thread_;
  std::mutex mutex_;
  int value_;
  int sink_;
};

class LambdaNotStarted {
 public:
  void call_lambda_ok() {
    auto body = [this] { sink_ = value_; };
    body();
  }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  std::mutex mutex_;
  int value_;
  int sink_;
};

class NoLocks {
 public:
  void start() { thread_ = std::thread(&NoLocks::run_ok, this); }

  void set(int v) { value_ = v; }

 private:
  void run_ok() { sink_ = value_; }

  std::thread thread_;
  int value_;
  int sink_;
};

class PthreadTrampoline {
 public:
  void start() {
    pthread_create(&thread_, nullptr, &PthreadTrampoline::entry, this);
  }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  static void* entry(void* self) {
    static_cast<PthreadTrampoline*>(self)->FN_run_bad();
    return nullptr;
  }

  // the accesses through the argument of [entry] are not matched with the
  // accesses through [this] in the other methods
  void FN_run_bad() { sink_ = value_; }

  pthread_t thread_;
  std::mutex mutex_;
  int value_;
  int sink_;
};

class ThreadPool {
 public:
  void start() { workers_.emplace_back(&ThreadPool::run_bad, this); }

  // the race is reported in the lambda
  void start_lambda_bad() {
    workers_.emplace_back([this] { sink_ = value_; });
  }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  void run_bad() { sink_ = value_; }

  std::vector<std::thread> workers_;
  std::mutex mutex_;
  int value_;
  int sink_;
};

class DeferredAsync {
 public:
  // the task runs when get() is called, under the lock
  int get_ok() {
    auto future = std::async(std::launch::deferred, [this] { return value_; });
    std::lock_guard<std::mutex> lock(mutex_);
    return future.get();
  }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  std::mutex mutex_;
  int value_;
};

class DeferredPolicyVariable {
 public:
  int get() {
    auto policy = std::launch::deferred;
    auto future = std::async(policy, &DeferredPolicyVariable::FP_run_ok, this);
    std::lock_guard<std::mutex> lock(mutex_);
    return future.get();
  }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  // only a constant std::launch::deferred policy is recognised
  int FP_run_ok() { return value_; }

  std::mutex mutex_;
  int value_;
};

class ForkJoin {
 public:
  // the join is not modelled; RacerD also reports the read after a write under
  // the lock in the same method when there is no thread
  int FP_compute_ok() {
    std::thread thread([this] {
      std::lock_guard<std::mutex> lock(mutex_);
      result_ = 42;
    });
    thread.join();
    return result_;
  }

 private:
  std::mutex mutex_;
  int result_;
};

class WriteBeforeStart {
 public:
  void start() {
    {
      std::lock_guard<std::mutex> lock(mutex_);
      config_ = 1;
    }
    thread_ = std::thread(&WriteBeforeStart::FP_run_ok, this);
  }

 private:
  // the start of the thread is not modelled as ordering the write in start()
  // before the read
  void FP_run_ok() { sink_ = config_; }

  std::thread thread_;
  std::mutex mutex_;
  int config_;
  int sink_;
};

class OwnedByThread {
 public:
  OwnedByThread() { thread_ = std::thread(&OwnedByThread::FP_run_ok, this); }

  void push(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    backlog_ = v;
  }

 private:
  void consume() {
    std::lock_guard<std::mutex> lock(mutex_);
    queue_ = backlog_;
  }

  // only this thread accesses queue_, but a routine started as a thread is
  // assumed to run in parallel with itself
  void FP_run_ok() {
    consume();
    sink_ = queue_;
  }

  std::thread thread_;
  std::mutex mutex_;
  int backlog_;
  int queue_;
  int sink_;
};

std::mutex copy_mutex;

class CopyCapture {
 public:
  // the lambda reads a copy of the object
  void start_copy_ok() {
    std::thread([*this] { return value_; }).detach();
  }

  void set(int v) {
    std::lock_guard<std::mutex> lock(copy_mutex);
    value_ = v;
  }

 private:
  int value_;
};

class TemplatedWrapper {
 public:
  // the routine started by submit() is a parameter, not a known lambda
  void FN_start_lambda_bad() {
    submit([this] { sink_ = value_; });
  }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  template <class F>
  void submit(F f) {
    thread_ = std::thread(f);
  }

  std::thread thread_;
  std::mutex mutex_;
  int value_;
  int sink_;
};

class ChosenRoutine {
 public:
  void start(bool b) {
    auto routine =
        b ? &ChosenRoutine::FN_run_bad : &ChosenRoutine::FN_other_bad;
    thread_ = std::thread(routine, this);
  }

  void set(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    value_ = v;
  }

 private:
  // a routine chosen between two functions is not tracked
  void FN_run_bad() { sink_ = value_; }

  void FN_other_bad() { sink_ = value_ + 1; }

  std::thread thread_;
  std::mutex mutex_;
  int value_;
  int sink_;
};

} // namespace thread_entry
