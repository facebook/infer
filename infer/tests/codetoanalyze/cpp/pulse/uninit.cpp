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

  void call_get_MyClass_ok() { int x = get_MyClass().i; }

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

  void call_get_MyClass_param_ok(my_enum my_enum) {
    int x = get_MyClass_param(my_enum).i;
  }

  MyClass get_MyClass_infeasible_default() {
    switch (get_my_enum()) {
      case (my_enum_1):
        return MyClass{1, 2};
        break;
      case (my_enum_2):
        return MyClass{1, 2};
        break;
      default:
        // infeasible
        break;
    }
  }

  void call_get_MyClass_infeasible_default_ok() {
    int x = get_MyClass_infeasible_default().i;
  }

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wreturn-type"
  MyClass get_MyClass_feasible_default() {
    switch (get_my_enum()) {
      case (my_enum_1):
        return MyClass{1, 2};
        break;
      default:
        // feasible
        break;
    }
  }
#pragma clang diagnostic pop

  void call_get_MyClass_feasible_default_bad() {
    int x = get_MyClass_feasible_default().i;
  }

  std::function<MyClass()> unknown;

  MyClass havoc_return_param() { return unknown(); }

  int call_havoc_return_param_ok() {
    MyClass x = havoc_return_param();
    return x.i;
  }
};

class Uninit2 {
 public:
  int f1;
  int f2;

  void may_read_f1_empty(std::string s) {
    if (s.empty()) {
      int x = f1;
    }
  }

  void not_read_f1_ok() {
    Uninit2 o;
    o.may_read_f1_empty("non empty string");
  }

  void read_f1_bad() {
    Uninit2 o;
    o.may_read_f1_empty(std::string());
  }

  void may_read_f2_length(std::string s) {
    if (s.length() == 0) {
      int x = f2;
    }
  }

  void not_read_f2_ok() {
    Uninit2 o;
    o.may_read_f2_length("non empty string");
  }

  void read_f2_bad() {
    Uninit2 o;
    o.may_read_f2_length("");
  }
};

void unknown_call_lambda(std::function<void()> f);

int init_by_capture_good() {
  int x;
  unknown_call_lambda([&]() { x = 42; });
  return x;
}

void init_param(int* p) { p[0] = 42; }

int init_in_callee_ok() {
  int x;
  init_param(&x);
  return x;
}

class Nested {
 public:
  int i;
  Uninit2 mc;
};

void unknown_init_nested(Nested& x);

int read_nested(Nested& x) {
  unknown_init_nested(x);
  return x.mc.f1;
}

int call_read_nested_ok() {
  Nested x;
  return read_nested(x);
}

class Uninit3 {
 public:
  int f1;
  int f2;
};

class Uninit4 {
  Uninit3& uninit3_;
  int x;

 public:
  Uninit4(Uninit3& uninit3) : uninit3_{uninit3} { Uninit3 dummy = uninit3_; }
};

void construct_unint4_ok(Uninit3 uninit3) { Uninit4 uninit4(uninit3); }

int dummy_func(int);

void comma_operator_ok() {
  int i, j;
  i = ({ dummy_func(42); }), j = ({ dummy_func(i); });
}
