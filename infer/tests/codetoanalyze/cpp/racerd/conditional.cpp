/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

class Conditional {
 public:
  Conditional() {}

  int get_x() { return x; }

  bool owns() {
    // temporary is properly destroyed and lock released
    return std::unique_lock<std::mutex>(mutex_).owns_lock();
  }

  void run_ok() {
    if (owns()) {
    }

    x = 0;
  }

  int get_y() { return y; }

  void run_FP() {
    // temporary not destroyed, so lock stays acquired at store to [x]
    if (std::unique_lock<std::mutex>(mutex_).owns_lock()) {
    }

    y = 0;
  }

 private:
  int x;
  int y;
  std::mutex mutex_;
};
