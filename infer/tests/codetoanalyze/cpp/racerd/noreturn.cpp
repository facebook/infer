/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

class Noreturn {
 public:
  Noreturn() {}

  int get_x_without_lock() { return x; }

  void write_not_under_lock() {
    { std::unique_lock<std::mutex> g(mutex_); }
    x = 0;
  }

  [[noreturn]] void never_returns();

  void indirectly_never_returns() { never_returns(); }

  void may_not_return(bool b) {
    if (b)
      never_returns();
  }

  void never_returns_under_lock() {
    std::unique_lock<std::mutex> g(mutex_);
    never_returns();
  }

  void write_after_never_returns_under_lock() {
    never_returns_under_lock();
    x = 0;
  }

  void write_after_indirect_never_returns_under_lock() {
    std::unique_lock<std::mutex> g(mutex_);
    indirectly_never_returns();
    x = 0;
  }

  int get_y_without_lock() { return y; }

  void write_after_call_to_may_never_return_under_lock_bad(bool b) {
    std::unique_lock<std::mutex> g(mutex_);
    may_not_return(b);
    y = 0;
  }

 private:
  int x;
  int y;
  std::mutex mutex_;
};
