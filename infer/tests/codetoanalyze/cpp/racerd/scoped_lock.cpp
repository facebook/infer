/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

class ScopedLock {
 public:
  ScopedLock() {}

  void store_x(int xx) {
    std::scoped_lock<std::mutex> g(mutex_);
    x = xx;
  }

  int get_x() {
    std::scoped_lock<std::mutex> g(mutex_);
    return x;
  }

  void store_y(int yy) {
    std::scoped_lock<std::mutex> g(mutex_);
    y = yy;
  }

  int get_y_bad() { return y; }

  void store_z(int zz) { z = zz; }

  int get_z() {
    std::scoped_lock<std::mutex> g(mutex_);
    return z;
  }

 private:
  int x, y, z;
  std::mutex mutex_;
};
