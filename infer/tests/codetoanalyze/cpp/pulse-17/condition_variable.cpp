/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <chrono>
#include <condition_variable>
#include <functional>
#include <mutex>
#include <optional>

namespace condition_variable {

struct Job {
  int id;
};

class Worker {
 public:
  int wait_ok() {
    std::unique_lock<std::mutex> lock(mutex_);
    job_ = nullptr;
    cv_.wait(lock, [this] { return job_ != nullptr; });
    return job_->id;
  }

  int no_wait_bad() {
    std::unique_lock<std::mutex> lock(mutex_);
    job_ = nullptr;
    return job_->id;
  }

  int wait_for_ok() {
    std::unique_lock<std::mutex> lock(mutex_);
    job_ = nullptr;
    if (!cv_.wait_for(lock, std::chrono::seconds(1), [this] {
          return job_ != nullptr;
        })) {
      return -1;
    }
    return job_->id;
  }

  // Pulse cannot apply the summary of a lambda that captures [this], so the
  // wait havocs [*this] instead of returning the value of the predicate
  int FN_wait_for_timeout_bad() {
    std::unique_lock<std::mutex> lock(mutex_);
    job_ = nullptr;
    if (!cv_.wait_for(lock, std::chrono::seconds(1), [this] {
          return job_ != nullptr;
        })) {
      return job_->id;
    }
    return 0;
  }

  int wait_until_ok(std::chrono::steady_clock::time_point deadline) {
    std::unique_lock<std::mutex> lock(mutex_);
    job_ = nullptr;
    if (!cv_.wait_until(lock, deadline, [this] { return job_ != nullptr; })) {
      return -1;
    }
    return job_->id;
  }

  int condition_variable_any_ok() {
    std::unique_lock<std::mutex> lock(mutex_);
    job_ = nullptr;
    any_cv_.wait(lock, [this] { return job_ != nullptr; });
    return job_->id;
  }

  int leak_after_wait_bad() {
    std::unique_lock<std::mutex> lock(mutex_);
    job_ = nullptr;
    cv_.wait(lock, [this] { return job_ != nullptr; });
    int* p = new int(job_->id);
    return *p;
  }

  void wait_for_job() {
    std::unique_lock<std::mutex> lock(mutex_);
    cv_.wait(lock, [this] { return job_ != nullptr; });
  }

  void wait_then_clear() {
    std::unique_lock<std::mutex> lock(mutex_);
    cv_.wait(lock, [this] { return job_ != nullptr; });
    job_ = nullptr;
  }

  // the closure stored in [on_job_] refers to the [this] variable, which the
  // wait must not re-bind
  void set_callback_then_wait_then_clear() {
    std::unique_lock<std::mutex> lock(mutex_);
    on_job_ = [this] { job_->id++; };
    cv_.wait(lock, [this] { return job_ != nullptr; });
    job_ = nullptr;
  }

  void submit_then_wait_ok() {
    std::unique_lock<std::mutex> lock(mutex_);
    Job* job = new Job{0};
    job_ = job;
    cv_.wait(lock, [this, job] { return job_ != job; });
  }

  std::mutex mutex_;
  std::condition_variable cv_;
  std::condition_variable_any any_cv_;
  std::function<void()> on_job_;
  Job* job_ = nullptr;
};

int call_wait_for_job_ok(Worker& worker) {
  worker.job_ = nullptr;
  worker.wait_for_job();
  return worker.job_->id;
}

int call_wait_then_clear_bad(Worker& worker) {
  worker.wait_then_clear();
  return worker.job_->id;
}

int call_set_callback_then_wait_then_clear_bad(Worker& worker) {
  worker.set_callback_then_wait_then_clear();
  return worker.job_->id;
}

int captured_local_ok(std::mutex& m, std::condition_variable& cv) {
  bool done = false;
  Job* job = nullptr;
  std::unique_lock<std::mutex> lock(m);
  cv.wait(lock, [&] { return done; });
  if (!done) {
    return job->id;
  }
  return 0;
}

int captured_reference_ok(std::mutex& m,
                          std::condition_variable& cv,
                          Job*& job) {
  std::unique_lock<std::mutex> lock(m);
  job = nullptr;
  cv.wait(lock, [&] { return job != nullptr; });
  return job->id;
}

// the wait only havocs what the predicate reads, but other threads can also
// change [job] while the lock is released
int FP_state_not_read_by_predicate_ok(std::mutex& m,
                                      std::condition_variable& cv,
                                      bool& ready,
                                      Job*& job) {
  std::unique_lock<std::mutex> lock(m);
  job = nullptr;
  ready = false;
  cv.wait(lock, [&] { return ready; });
  return job->id;
}

// the frontend translates the closure of a generic lambda without its
// instantiated call operator, so the wait can neither call the predicate nor
// havoc what it captures
int FP_generic_lambda_predicate_ok(std::mutex& m,
                                   std::condition_variable& cv,
                                   Job*& job) {
  std::unique_lock<std::mutex> lock(m);
  job = nullptr;
  cv.wait(lock, [&](auto&&...) { return job != nullptr; });
  return job->id;
}

int pointer_captured_by_value_ok(std::mutex& m,
                                 std::condition_variable& cv,
                                 Job** slot) {
  *slot = nullptr;
  std::unique_lock<std::mutex> lock(m);
  cv.wait(lock, [slot] { return *slot != nullptr; });
  return (*slot)->id;
}

void pointer_captured_by_value_leak_bad(std::mutex& m,
                                        std::condition_variable& cv) {
  bool* ready = new bool(false);
  std::unique_lock<std::mutex> lock(m);
  cv.wait(lock, [ready] { return *ready; });
}

struct Holder {
  Job* job;
};

void pointer_owned_by_captured_object_ok(std::mutex& m,
                                         std::condition_variable& cv,
                                         Holder* holder) {
  Job* job = new Job{0};
  holder->job = job;
  std::unique_lock<std::mutex> lock(m);
  cv.wait(lock,
          [holder, job] { return job->id != 0 || holder->job == nullptr; });
}

struct HasJob {
  Job** slot;
  bool operator()() const { return *slot != nullptr; }
};

int function_object_ok(std::mutex& m, std::condition_variable& cv, Job** slot) {
  *slot = nullptr;
  std::unique_lock<std::mutex> lock(m);
  cv.wait(lock, HasJob{slot});
  return (*slot)->id;
}

int std_function_predicate_dereferences_null_bad(std::mutex& m,
                                                 std::condition_variable& cv) {
  Job* job = nullptr;
  std::unique_lock<std::mutex> lock(m);
  std::function<bool()> has_job = [job] { return job->id > 0; };
  cv.wait(lock, has_job);
  return 0;
}

int wait_for_returns_predicate_ok(std::mutex& m, std::condition_variable& cv) {
  bool done = false;
  std::unique_lock<std::mutex> lock(m);
  bool woken = cv.wait_for(lock, std::chrono::seconds(1), [&] { return done; });
  if (woken != done) {
    int* p = nullptr;
    return *p;
  }
  return 0;
}

int wait_until_captured_local_ok(
    std::mutex& m,
    std::condition_variable_any& cv,
    std::chrono::steady_clock::time_point deadline) {
  Job* job = nullptr;
  std::unique_lock<std::mutex> lock(m);
  if (cv.wait_until(lock, deadline, [&] { return job != nullptr; })) {
    return job->id;
  }
  return 0;
}

int wait_for_timeout_bad(std::mutex& m, std::condition_variable& cv) {
  Job* job = nullptr;
  std::unique_lock<std::mutex> lock(m);
  if (!cv.wait_for(
          lock, std::chrono::seconds(1), [&] { return job != nullptr; })) {
    return job->id;
  }
  return 0;
}

int lock_keeps_mutex_ok(std::mutex& m, std::condition_variable& cv) {
  bool done = false;
  std::unique_lock<std::mutex> lock(m);
  cv.wait(lock, [&] { return done; });
  if (lock.mutex() != &m) {
    int* p = nullptr;
    return *p;
  }
  return 0;
}

bool lock_passed_by_value_ok(std::condition_variable& cv,
                             std::unique_lock<std::mutex> lock,
                             bool& done) {
  cv.wait(lock, [&] { return done; });
  return done;
}

int predicate_dereferences_null_bad(std::mutex& m,
                                    std::condition_variable& cv) {
  Job* job = nullptr;
  std::unique_lock<std::mutex> lock(m);
  cv.wait(lock, [job] { return job->id > 0; });
  return 0;
}

int predicate_holds_on_entry_leak_bad(std::mutex& m,
                                      std::condition_variable& cv) {
  int* p = new int(42);
  std::unique_lock<std::mutex> lock(m);
  cv.wait(lock, [&] { return p != nullptr; });
  return *p;
}

bool global_ready;

// as far as Pulse knows the predicate never holds, but the code after the wait
// is still analyzed
int global_predicate_leak_bad(std::mutex& m, std::condition_variable& cv) {
  std::unique_lock<std::mutex> lock(m);
  global_ready = false;
  cv.wait(lock, [] { return global_ready; });
  int* p = new int(42);
  return *p;
}

Job* global_job;

// the wait does not havoc the globals that the predicate reads
int FP_global_predicate_ok(std::mutex& m, std::condition_variable& cv) {
  std::unique_lock<std::mutex> lock(m);
  global_job = nullptr;
  cv.wait(lock, [] { return global_job != nullptr; });
  return global_job->id;
}

class OptionalWorker {
 public:
  int wait_ok() {
    std::unique_lock<std::mutex> lock(mutex_);
    result_ = std::nullopt;
    cv_.wait(lock, [this] { return result_.has_value(); });
    return result_.value();
  }

  int no_wait_bad() {
    std::unique_lock<std::mutex> lock(mutex_);
    result_ = std::nullopt;
    return result_.value();
  }

 private:
  std::mutex mutex_;
  std::condition_variable cv_;
  std::optional<int> result_;
};

int captured_optional_reference_ok(std::mutex& m,
                                   std::condition_variable& cv,
                                   std::optional<int>& result) {
  std::unique_lock<std::mutex> lock(m);
  result = std::nullopt;
  cv.wait(lock, [&] { return result.has_value(); });
  return *result;
}

} // namespace condition_variable
