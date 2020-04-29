/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace constructor_formals {
class Basic {
 public:
  // there can be a race here between the initializer read and the set function
  // below
  Basic(Basic& other) : field_(other.field_) {}

  void FN_set_under_lock_bad(int value) {
    mutex_.lock();
    field_ = value;
    mutex_.unlock();
  }

 private:
  std::mutex mutex_;
  int field_;
};
} // namespace constructor_formals
