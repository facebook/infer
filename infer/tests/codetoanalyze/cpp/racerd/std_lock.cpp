/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace basics {

class StdLock {
 public:
  StdLock() {}

  void set_ok(StdLock* other) {
    std::lock(mutex_, other->mutex_);
    guarded = other->guarded;
  }

  int get_ok() {
    mutex_.lock();
    return guarded;
  }

  void set_bad(StdLock* other) {
    std::lock(mutex_, other->mutex_);
    not_guarded = other->not_guarded;
  }

  int get_bad() { return not_guarded; }

 private:
  int guarded;
  int not_guarded;
  std::mutex mutex_;
};
} // namespace basics
