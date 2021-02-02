/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <atomic>
#include <functional>

void get_closure(std::function<int()> closure);

class Uninit {
  void closure_call_ok() {
    auto closure = [this]() { return 5; };
    get_closure(closure);
  }

  class MyClass {
   public:
    int i;
    int j;
  };

  void init_by_store(MyClass* x) {
    MyClass y{3, 5};
    reinterpret_cast<std::atomic<MyClass>*>(x)->store(
        y, std::memory_order_release);
  }

  void call_init_by_store_ok() {
    MyClass x;
    init_by_store(&x);
    int y = x.i;
  }
};
