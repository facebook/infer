/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include <functional>

struct S {
  int f;

  S() { f = 1; }
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

int FN_reassign_lambda_capture_destroy_invoke_bad() {
  std::function<int()> f;
  {
    auto s = S();
    auto tmp = [&] { return s.f; };
    f = tmp;
  }
  return f();
}

// frontend doesn't understand difference between capture-by-value and
// capture-by-ref, need to fix
int value_capture_destroy_invoke_ok() {
  std::function<int()> f;
  {
    S s;
    f = [s] { return s.f; };
  }
  return f();
}

// same thing here
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

// we'll miss this because we count invoking a lambda object as a use of its
// captured vars, not the lambda object itself.
void FN_delete_lambda_then_call_bad() {
  auto lambda = [] { return 1; };
  ~lambda();
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
