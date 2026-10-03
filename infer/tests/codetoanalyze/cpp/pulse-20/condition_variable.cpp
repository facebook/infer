/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <chrono>
#include <condition_variable>
#include <mutex>
#include <stop_token>

extern int __infer_taint_source();
extern void __infer_taint_sink(int);

namespace condition_variable {

struct Job {
  int id;
};

int wait_stop_token_ok(std::mutex& m,
                       std::condition_variable_any& cv,
                       const std::stop_token& stop) {
  Job* job = nullptr;
  std::unique_lock<std::mutex> lock(m);
  if (!cv.wait(lock, stop, [&] { return job != nullptr; })) {
    return -1;
  }
  return job->id;
}

int wait_for_stop_token_ok(std::mutex& m,
                           std::condition_variable_any& cv,
                           const std::stop_token& stop) {
  Job* job = nullptr;
  std::unique_lock<std::mutex> lock(m);
  if (!cv.wait_for(lock, stop, std::chrono::seconds(1), [&] {
        return job != nullptr;
      })) {
    return -1;
  }
  return job->id;
}

int wait_until_stop_token_ok(std::mutex& m,
                             std::condition_variable_any& cv,
                             const std::stop_token& stop,
                             std::chrono::steady_clock::time_point deadline) {
  Job* job = nullptr;
  std::unique_lock<std::mutex> lock(m);
  if (!cv.wait_until(lock, stop, deadline, [&] { return job != nullptr; })) {
    return -1;
  }
  return job->id;
}

int wait_stop_token_predicate_dereferences_null_bad(
    std::mutex& m,
    std::condition_variable_any& cv,
    const std::stop_token& stop) {
  Job* job = nullptr;
  std::unique_lock<std::mutex> lock(m);
  return cv.wait(lock, stop, [job] { return job->id > 0; });
}

struct Message {
  bool ready;
  int value;
};

void wait_predicate_holds_keeps_taint_bad(std::mutex& m,
                                          std::condition_variable& cv,
                                          Message& msg) {
  std::unique_lock<std::mutex> lock(m);
  msg.ready = true;
  msg.value = __infer_taint_source();
  cv.wait(lock, [&] { return msg.ready; });
  __infer_taint_sink(msg.value);
}

// the wait havocs what the predicate reaches, which drops the taint of the
// values it replaces, as an unknown call taking [&msg] would
void FN_wait_keeps_taint_bad(std::mutex& m,
                             std::condition_variable& cv,
                             Message& msg) {
  std::unique_lock<std::mutex> lock(m);
  msg.ready = false;
  msg.value = __infer_taint_source();
  cv.wait(lock, [&] { return msg.ready; });
  __infer_taint_sink(msg.value);
}

} // namespace condition_variable
