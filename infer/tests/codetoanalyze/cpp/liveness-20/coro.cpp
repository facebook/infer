/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// make tests compatible with clang-12 for the moment
#include <coroutine>

namespace coro {

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
  void return_void();
  void unhandled_exception() {}
};

Task<void> dummy(const int&);

Task<void> use_live_var_ok(int x) {
  const auto& live_var = x;
  co_return co_await dummy(live_var);
}

} // namespace coro
