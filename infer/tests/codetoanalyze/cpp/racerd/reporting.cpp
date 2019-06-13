/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace reporting {

struct X {
  int w;
  X* x1;
};

class Basic {
 public:
  Basic() {}

  void test(X& xparam) { xparam.x1->w++; }

  void call1() {
    test(x); // race
  }

  int test_lock() {
    mutex_.lock();
    call1();
  }

  int test_unlock() { call1(); }

 private:
  X x;
  std::mutex mutex_;
};
} // namespace reporting
