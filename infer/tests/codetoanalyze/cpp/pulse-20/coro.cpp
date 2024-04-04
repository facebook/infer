/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// The test is not compatible with clang-12.
#include <coroutine>

namespace coro {

/* a simplistic Task promise: framework code (this is possibly the worst
   coroutine code ever written, its only purpose is to make the client code
   compile, do not use...) */

template <typename T>
struct Task;

template <typename T>
struct Task {
  using promise_type = Task<T>;

  bool await_ready() noexcept;
  std::coroutine_handle<> await_suspend(
      std::coroutine_handle<Task<T>> coro) noexcept;
  T await_resume() noexcept;
  Task<T> get_return_object();
  std::suspend_always initial_suspend() noexcept { return {}; }
  std::suspend_always final_suspend() noexcept { return {}; }
  void return_value(T&& value);
  std::suspend_always yield_value(T&& value);
  void unhandled_exception() {}
};

/* client code */

// test that infer "sees" code in coroutines

Task<int> task42() { co_return 42; }

Task<int> task43() { co_return (co_await task42()) + 1; }

Task<int> do_coroutine_stuff_then_npe_bad() {
  int* p = nullptr;
  int fortytwo = co_await task42();
  int fortythree = co_await task43();
  co_yield fortytwo + fortythree;
  *p = 42;
  co_return 10;
}

Task<int> FP_do_coroutine_stuff_then_npe_unless_semantics_accurate_bad() {
  int* p = nullptr;
  int fortytwo = co_await task42();
  int fortythree = co_await task43();
  co_yield fortytwo + fortythree;
  if (fortytwo != 42 || fortythree != 43) {
    *p = 42;
  }
  co_return 10;
}

// taint tests

// an arbitrary datatype, not that it matters

struct Request {
  ~Request() {}
};

// sink
void requestSink(Request* request);
// source
Task<Request*> makeRequest();

// intermediate coroutine
Task<Request*> co_get_something(Request* request) {
  requestSink(request);
  co_return new Request();
}

Task<Request*> makeRequestToSink_bad() {
  auto request = co_await makeRequest();
  co_return co_await co_get_something(request);
}

} // namespace coro
