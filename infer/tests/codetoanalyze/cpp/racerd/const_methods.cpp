/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace const_methods {

struct Inner {
  int f;
};

class ConstMethods {
 public:
  int get_const_bad() const { return x_; }

  void set_x(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    x_ = v;
  }

  int get_nonconst_bad() { return mutable_y_; }

  void set_mutable_y_in_const_method(int v) const {
    std::lock_guard<std::mutex> lock(mutex_);
    mutable_y_ = v;
  }

  int get_const_indirect_bad() const { return read_w(); }

  void set_w(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    w_ = v;
  }

  int get_const_nested_field_bad() const { return inner_.f; }

  void set_nested_field(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    inner_.f = v;
  }

  int get_const_locked_ok() const {
    std::lock_guard<std::mutex> lock(mutex_);
    return z_;
  }

  void set_z(int v) {
    std::lock_guard<std::mutex> lock(mutex_);
    z_ = v;
  }

  int get_const_not_guarded_ok() const { return not_guarded_; }

  void set_not_guarded(int v) { not_guarded_ = v; }

 private:
  int read_w() const { return w_; }

  int get_private_const_ok() const { return x_; }

  mutable std::mutex mutex_;
  int x_;
  mutable int mutable_y_;
  int w_;
  Inner inner_;
  int z_;
  int not_guarded_;
};
} // namespace const_methods
