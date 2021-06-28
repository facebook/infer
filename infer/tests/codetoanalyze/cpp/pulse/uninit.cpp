/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <atomic>
#include <functional>
#include <string>

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

class Uninit2 {
  int f1;
  int f2;

  void may_read_f1(std::string s) {
    if (s.empty()) {
      int x = f1;
    }
  }

  void not_read_f1_ok() {
    Uninit2 o;
    o.may_read_f1("non empty string");
  }

  void read_f1_bad() {
    Uninit2 o;
    o.may_read_f1(std::string());
  }

  void may_read_f2(std::string s) {
    if (s.length() == 0) {
      int x = f1;
    }
  }

  void not_read_f2_ok() {
    Uninit2 o;
    o.may_read_f2("non empty string");
  }

  void read_f2_bad() {
    Uninit2 o;
    o.may_read_f2("");
  }
};
