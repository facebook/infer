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

std::function<int()> ref_capture_read_lambda_ok() {
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

S& FN_ref_capture_return_local_lambda_bad() {
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
}

void call_lambda_bad() {
  auto f = [](S* s) { int x = s->f; };
  S* s = new S();
  delete s;
  f(s);
}
