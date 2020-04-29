/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace locals {

struct X {
  int f;
};

class Ownership {
 public:
  Ownership() {}

  int struct_ok() {
    X x;
    mutex_.lock();
    x.f = 7;
    mutex_.unlock();
    return x.f;
  }

  int ptr_to_struct_ok() {
    X* x = new X();
    mutex_.lock();
    x->f = 7;
    mutex_.unlock();
    return x->f;
  }

  int copy_constructor_ok() {
    X x = current; // copy constructor
    mutex_.lock();
    x.f = 7;
    mutex_.unlock();
    return x.f;
  }

  int FN_ptr_to_field_struct_bad() {
    X* x = &current;
    mutex_.lock();
    x->f = 7;
    mutex_.unlock();
    return x->f;
  }

  int copy_formal_ok(X xformal) {
    X x = xformal; // copy constructor
    mutex_.lock();
    x.f = 7;
    mutex_.unlock();
    return x.f;
  }

  int FN_ptr_to_formal_bad(X* xformal) {
    X* x = xformal;
    mutex_.lock();
    x->f = 7;
    mutex_.unlock();
    return x->f;
  }

 private:
  X current;
  std::mutex mutex_;
};
} // namespace locals
