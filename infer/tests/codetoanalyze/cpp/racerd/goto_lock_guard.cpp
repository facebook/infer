/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace goto_lock_guard {

class GotoLockGuard {
 public:
  GotoLockGuard() {}

  int goto_out_of_lock_scope_bad(int b, int new_value) {
    {
      std::lock_guard<std::mutex> lock(mutex_);
      if (b) {
        goto out;
      }
      read_after_goto = new_value;
    }
    return 0;
  out:
    return read_after_goto;
  }

  int goto_join_bad(int b, int new_value) {
    {
      std::lock_guard<std::mutex> lock(mutex_);
      if (b) {
        goto out;
      }
      read_after_join = new_value;
    }
  out:
    return read_after_join;
  }

  int goto_within_lock_scope_ok(int b, int new_value) {
    std::lock_guard<std::mutex> lock(mutex_);
    if (b) {
      goto out;
    }
    well_guarded = new_value;
  out:
    return well_guarded;
  }

  void write_with_goto(int b, int new_value) {
    {
      std::lock_guard<std::mutex> lock(mutex_);
      if (b) {
        goto out;
      }
      read_after_call = new_value;
    }
  out:
    return;
  }

  int read_after_call_bad(int b, int new_value) {
    write_with_goto(b, new_value);
    return read_after_call;
  }

 private:
  int read_after_goto;
  int read_after_join;
  int read_after_call;
  int well_guarded;
  std::mutex mutex_;
};
} // namespace goto_lock_guard
