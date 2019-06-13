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

  int test0_ok() {
    X x;
    mutex_.lock();
    x.f = 7;
    mutex_.unlock();
    return x.f;
  }

  int test1_ok() {
    X* x = new X();
    mutex_.lock();
    x->f = 7;
    mutex_.unlock();
    return x->f;
  }

  int test2_ok() {
    X x = current; // copy constructor
    mutex_.lock();
    x.f = 7;
    mutex_.unlock();
    return x.f;
  }

  int test2_bad() {
    X* x = &current;
    mutex_.lock();
    x->f = 7;
    mutex_.unlock();
    return x->f;
  }

  int test3_ok(X xformal) {
    X x = xformal; // copy constructor
    mutex_.lock();
    x.f = 7;
    mutex_.unlock();
    return x.f;
  }

  int test3_bad(X* xformal) {
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
