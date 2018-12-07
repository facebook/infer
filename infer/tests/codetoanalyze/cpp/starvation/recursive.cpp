/*
 * Copyright (c) 2018-present, Facebook, Inc.
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

  void FP_multi_ok() {
    std::lock_guard<std::recursive_mutex> l(recursive_mutex_);
    { std::lock_guard<std::recursive_mutex> l(recursive_mutex_); }
  }

  void FP_unknown_ok() {
    std::lock_guard<UnknownMutex> l(umutex_);
    { std::lock_guard<UnknownMutex> l(umutex_); }
  }

  void FP_path_sensitive_ok() {
    std::lock_guard<std::mutex> l(mutex_);
    bool flag = false;

    if (flag) {
      std::lock_guard<std::mutex> l(mutex_);
    }
  }

 private:
  std::recursive_mutex recursive_mutex_;
  std::mutex mutex_;
  UnknownMutex umutex_;
};
