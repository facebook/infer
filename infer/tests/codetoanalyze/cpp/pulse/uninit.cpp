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

enum my_enum {
  my_enum_1 = 0,
  my_enum_2 = 1,
};

extern my_enum get_my_enum();

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

  MyClass get_MyClass() {
    switch (get_my_enum()) {
      case (my_enum_1):
        return MyClass{1, 2};
        break;
      case (my_enum_2):
        return MyClass{1, 2};
        break;
    }
  }

  void FP_call_get_MyClass_ok() { int x = get_MyClass().i; }

  MyClass get_MyClass_param(my_enum my_enum) {
    switch (my_enum) {
      case (my_enum_1):
        return MyClass{1, 2};
        break;
      case (my_enum_2):
        return MyClass{1, 2};
        break;
    }
  }

  /* It has an latent infeasible issue, but it is better to have no issues. */
  void FP_call_get_MyClass_param_ok(my_enum my_enum) {
    int x = get_MyClass_param(my_enum).i;
  }

  MyClass get_MyClass_default() {
    switch (get_my_enum()) {
      case (my_enum_1):
        return MyClass{1, 2};
        break;
      case (my_enum_2):
        return MyClass{1, 2};
        break;
      default:
        return MyClass{1, 2};
        break;
    }
  }

  void call_get_MyClass_default_ok() { int x = get_MyClass_default().i; }
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
