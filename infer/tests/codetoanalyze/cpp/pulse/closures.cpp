/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <functional>

struct S {
  int f;

  S() { f = 1; }
  ~S() {}
};

int ref_capture_destroy_invoke_bad() {
  std::function<int()> f;
  {
    S s;
    f = [&s] { return s.f; };
  } // destructor for s called here
  return f(); // s used here
}

int implicit_ref_capture_destroy_invoke_bad() {
  std::function<int()> f;
  {
    auto s = S();
    f = [&] { return s.f; };
  }
  return f();
}

// FN in C++14
int reassign_lambda_capture_destroy_invoke_bad() {
  std::function<int()> f;
  {
    auto s = S();
    // this is a copy constructor in C++14, which pulse misses,
    // but it's a straight assignment in C++17, which pulse understands
    auto tmp = [&] { return s.f; };
    f = tmp;
  }
  return f();
}

int value_capture_destroy_invoke_ok() {
  std::function<int()> f;
  {
    S s;
    f = [s] { return s.f; };
  }
  return f();
}

int implicit_value_capture_destroy_invoke_ok() {
  std::function<int()> f;
  {
    S s;
    f = [=] { return s.f; };
  }
  return f();
}

int ref_capture_invoke_ok() {
  std::function<int()> f;
  int ret;
  {
    S s;
    f = [&s] { return s.f; };
    ret = f();
  }
  return ret;
}

void invoke_twice_ok() {
  std::function<int()> f;
  int ret;
  {
    S s;
    f = [&s] { return s.f; };
    f();
    f();
  }
}

void ref_capture_read_lambda_ok() {
  std::function<int()> f;
  int ret;
  {
    S s;
    f = [&s] { return s.f; };
  }
  auto tmp =
      f; // reading (but not invoking) the lambda doesn't use its captured vars
}

// explicit destructor call is not modelled
int FN_delete_lambda_then_call_bad() {
  std::function<int()> lambda = [] { return 1; };
  lambda.~function();
  return lambda();
}

// need to treat escaping as a use in order to catch this
std::function<int()> FN_ref_capture_return_lambda_bad() {
  std::function<int()> f;
  int ret;
  {
    S s;
    f = [&s] { return s.f; };
  }
  return f; // if the caller invokes the lambda, it will try to read the invalid
            // stack address
}

int ref_capture_return_local_lambda_ok() {
  S x;
  auto f = [&x](void) -> S& {
    // do not report this because there is a good chance that this function will
    // only be used in the local scope
    return x;
  };
  return f().f;
}

S& ref_capture_return_local_lambda_bad() {
  S x;
  auto f = [&x](void) -> S& {
    // no way to know if ok here
    return x;
  };
  // woops, this returns a ref to a local!
  return f();
}

struct C {
  int val() const;
  ~C();
};

struct D {
  void add(int v);
  ~D();
};

void capture_multiple_vars_by_value_ok(C c, C c2) {
  auto f = [=]() -> D* {
    auto d = new D();
    d->add(c.val());
    d->add(c2.val());
    return d;
  };
}

void call_lambda_ok() {
  auto f = [](S* s) { int x = s->f; };
  S* s = new S();
  f(s);
  delete s;
}

void call_lambda_bad() {
  auto f = [](S* s) { int x = s->f; };
  S* s = new S();
  delete s;
  f(s);
}

void call_lambda_std_fun_bad() {
  std::function<void(S*)> f;
  f = [](S* s) { int x = s->f; };
  S* s = new S();
  delete s;
  f(s);
}

void call_std_fun_constructor_bad() {
  std::function<void(S*)> f1 = [](S* s) { int x = s->f; };
  std::function<void(S*)> f2 = f1;
  S* s = new S();
  delete s;
  f2(s);
}

void function_constructor_null_ok() { std::function<int()> f = nullptr; }

void function_assign_null_ok() {
  std::function<int()> f = [] { return 1; };
  f = nullptr;
}

void capture_by_value_ok() {
  int value = 5;
  auto f = [value]() -> int* { return new int(value); };
  value++;
  int* p = f();
  int* q = nullptr;
  if (*p != 5) {
    *q = 42;
  }
  delete p;
}

void capture_by_value_bad() {
  int value = 5;
  auto f = [value]() -> int* { return new int(value); };
  value++;
  int* p = f();
  int* q = nullptr;
  if (*p == 5) {
    *q = 42;
  }
  delete p;
}

void capture_by_ref_ok() {
  int value = 5;
  auto f = [&value]() -> int* { return new int(value); };
  value++;
  int* p = f();
  int* q = nullptr;
  if (*p != 6) {
    *q = 42;
  }
  delete p;
}

void capture_by_ref_bad() {
  int value = 5;
  auto f = [&value]() -> int* { return new int(value); };
  value++;
  int* p = f();
  int* q = nullptr;
  if (*p == 6) {
    *q = 42;
  }
  delete p;
}

void capture_by_value_init_ok() {
  int value = 5;
  auto f = [v = value]() -> int* { return new int(v); };
  value++;
  int* p = f();
  int* q = nullptr;
  if (*p != 5) {
    *q = 42;
  }
  delete p;
}

void capture_by_value_init_bad() {
  int value = 5;
  auto f = [v = value]() -> int* { return new int(v); };
  value++;
  int* p = f();
  int* q = nullptr;
  if (*p == 5) {
    *q = 42;
  }
  delete p;
}

void capture_by_ref_init_ok() {
  int value = 5;
  auto f = [&v = value]() -> int* { return new int(v); };
  value++;
  int* p = f();
  int* q = nullptr;
  if (*p != 6) {
    *q = 42;
  }
  delete p;
}

void capture_by_ref_init_bad() {
  int value = 5;
  auto f = [&v = value]() -> int* { return new int(v); };
  value++;
  int* p = f();
  int* q = nullptr;
  if (*p == 6) {
    *q = 42;
  }
  delete p;
}

void ref_capture_by_value_ok() {
  int value = 5;
  int& ref = value;
  auto f = [ref]() -> int* { return new int(ref); };
  ref++;
  int* p = f();
  int* q = nullptr;
  if (*p != 5) {
    *q = 42;
  }
  delete p;
}

void ref_capture_by_value_bad() {
  int value = 5;
  int& ref = value;
  auto f = [ref]() -> int* { return new int(ref); };
  ref++;
  int* p = f();
  int* q = nullptr;
  if (*p == 5) {
    *q = 42;
  }
  delete p;
}

void ref_capture_by_ref_ok() {
  int value = 5;
  int& ref = value;
  auto f = [&ref]() -> int* { return new int(ref); };
  ref++;
  int* p = f();
  int* q = nullptr;
  if (*p != 6) {
    *q = 42;
  }
  delete p;
}

void ref_capture_by_ref_bad() {
  int value = 5;
  int& ref = value;
  auto f = [&ref]() -> int* { return new int(ref); };
  ref++;
  int* p = f();
  int* q = nullptr;
  if (*p == 6) {
    *q = 42;
  }
  delete p;
}

void struct_capture_by_ref_bad() {
  S s;
  auto f = [&s]() -> int* { return new int(s.f); };
  s.f = 5;
  int* p = f();
  int* q = nullptr;
  if (*p == 5) {
    *q = 42;
  }
  delete p;
}

void struct_capture_by_ref_ok() {
  S s;
  auto f = [&s]() -> int* { return new int(s.f); };
  s.f = 5;
  int* p = f();
  int* q = nullptr;
  if (*p != 5) {
    *q = 42;
  }
  delete p;
}

void struct_capture_by_val_bad() {
  S s;
  auto f = [s]() -> int* { return new int(s.f); };
  s.f = 5;
  int* p = f();
  int* q = nullptr;
  if (*p == 1) {
    *q = 42;
  }
  delete p;
}

void struct_capture_by_val_ok_FP() {
  S s;
  auto f = [s]() -> int* { return new int(s.f); };
  s.f = 5;
  int* p = f();
  int* q = nullptr;
  if (*p != 1) {
    *q = 42;
  }
  delete p;
}

S* update_inside_lambda_capture_and_init(S* s) {
  S* object = nullptr;
  auto f = [&o = object](S* s) { o = s; };
  f(s);
  return object;
}

int update_inside_lambda_capture_and_init_ok(S* param_s) {
  return update_inside_lambda_capture_and_init(param_s)->f;
}

S* update_inside_lambda_capture_only(S* s) {
  S* object = nullptr;
  /* FIXME: clang AST gives us `S*` for  variable `object` in the
     lambda's body, hence the translation misses one dereference */
  auto f = [&object](S* s) { object = s; };
  f(s);
  return object;
}

int update_inside_lambda_capture_only_ok(S* param_s) {
  return update_inside_lambda_capture_only(param_s)->f;
}

void call_argument(std::function<void(S*)> f, S* s) { f(s); }

S* update_inside_lambda_as_argument(S* s) {
  S* object = nullptr;
  auto f = [&o = object](S* s) { o = s; };
  call_argument(f, s);
  return object;
}

int update_inside_lambda_as_argument_ok(S* param_s) {
  return update_inside_lambda_as_argument(param_s)->f;
}

std::function<void()> get_lambda(bool b) {
  return [b]() -> void { return; };
}

void capture_false_by_value_ok() {
  const auto& f = get_lambda(false);
  f();
}

void FP_update_inside_lambda_visible_outside_ok() {
  int x = 0; // there are two variables x in the symbolic state `roots={ &x=v3,
             // &x=v1 }`
  auto f = [&xx = x]() {
    xx++;
  }; // one variable disappear because of dealloc instruction
  x = 7; // again there are two variables x in the symbolic state
  f();
  if (x != 8) {
    int* p = nullptr;
    *p = 42;
  }
}
