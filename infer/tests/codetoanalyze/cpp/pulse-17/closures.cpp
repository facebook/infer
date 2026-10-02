/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void generic_lambda_ok_FP() {
  int x = 1;
  [&](auto y) { x += y; }(3);
  if (x != 4) {
    int* p = nullptr;
    *p = 42;
  }
}

int call_lambda_through_generic_lambda() {
  int c = 42;
  auto call_lambda = [](auto lambda) { return lambda(100); };
  return call_lambda([c](int a) { return a + c; });
}

int call_lambda_through_generic_lambda_test_bad() {
  if (call_lambda_through_generic_lambda() == 142) {
    int* p = nullptr;
    return *p;
  }
  return 0;
}

int call_lambda_through_generic_lambda_test_good() {
  if (call_lambda_through_generic_lambda() == 143) {
    int* p = nullptr;
    return *p;
  }
  return 0;
}

int call_lambda_capturing_null_through_generic_lambda_bad() {
  int* p = nullptr;
  auto call_lambda = [](auto lambda) { return lambda(100); };
  return call_lambda([p](int a) { return a + *p; });
}
