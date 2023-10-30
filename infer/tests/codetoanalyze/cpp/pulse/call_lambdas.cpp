/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <functional>
#include <iostream>

template <typename F>
int call_lambda(F lambda) {
  return lambda(100);
}

int call_lambda_through_function() {
  int c = 42;
  return call_lambda([c](int a) { return a + c; });
}

int call_lambda_directly() {
  int c = 42;
  return [c](int a) { return a + c; }(100);
}

int call_lambda_through_function_test_bad() {
  if (call_lambda_through_function() == 142) {
    int* p = NULL;
    return *p;
  }
}

int call_lambda_through_function_test_good() {
  if (call_lambda_through_function() == 143) {
    int* p = NULL;
    return *p;
  }
}

int call_lambda_directly_test_bad() {
  if (call_lambda_directly() == 142) {
    int* p = NULL;
    return *p;
  }
}

int call_lambda_directly_test_good() {
  if (call_lambda_directly() == 143) {
    int* p = NULL;
    return *p;
  }
}

int call_std_fun_constructor() {
  int c = 42;
  std::function<int(int)> f = [c](int a) { return a + c; };
  return f(100);
}

int call_std_fun_constructor_test_bad() {
  if (call_std_fun_constructor() == 142) {
    int* p = NULL;
    return *p;
  }
}

int call_std_fun_constructor_test_good() {
  if (call_std_fun_constructor() == 143) {
    int* p = NULL;
    return *p;
  }
}

int call_lambda_after_copy() {
  int c = 41;
  int d = 1;
  auto f = [c, &d](int a) { return a + c + d; };
  auto g = f;
  return g(100);
}

int call_lambda_after_copy_test_bad() {
  if (call_lambda_after_copy() == 142) {
    int* p = NULL;
    return *p;
  }
}

int call_lambda_after_copy_test_good() {
  if (call_lambda_after_copy() == 143) {
    int* p = NULL;
    return *p;
  }
}
