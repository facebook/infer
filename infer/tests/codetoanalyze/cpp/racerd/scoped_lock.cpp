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

  void store_v(int vv) {
    std::scoped_lock g(mutex_, other_mutex_);
    v = vv;
  }

  int get_v() {
    std::scoped_lock g(mutex_, other_mutex_);
    return v;
  }

  void store_w(int ww) {
    std::scoped_lock g(mutex_, other_mutex_);
    w = ww;
  }

  int get_w_bad() { return w; }

  void store_u(int uu) {
    std::scoped_lock<std::mutex> g(mutex_);
    u = uu;
  }

  int get_u() {
    std::lock(mutex_, other_mutex_);
    std::scoped_lock g(std::adopt_lock, mutex_, other_mutex_);
    return u;
  }

  int get_u_after_release_bad() {
    {
      std::lock(mutex_, other_mutex_);
      std::scoped_lock g(std::adopt_lock, mutex_, other_mutex_);
    }
    return u;
  }

 private:
  int x, y, z, v, w, u;
  std::mutex mutex_;
  std::mutex other_mutex_;
};
