/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

struct UnknownMutex {
  UnknownMutex() {}

  void lock() {}

  void unlock() {}

  UnknownMutex(const UnknownMutex&) = delete;
  UnknownMutex& operator=(const UnknownMutex&) = delete;
};

class Recursive {
 public:
  Recursive() {}

  void multi_ok() {
    std::lock_guard<std::recursive_mutex> l(recursive_mutex_);
    { std::lock_guard<std::recursive_mutex> l(recursive_mutex_); }
  }

  void unknown_ok() {
    std::lock_guard<UnknownMutex> l(umutex_);
    { std::lock_guard<UnknownMutex> l(umutex_); }
  }

 private:
  std::recursive_mutex recursive_mutex_;
  UnknownMutex umutex_;
};
