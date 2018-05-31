/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

int test_volatile1() {
  std::unique_ptr<volatile int> x;
  std::unique_ptr<volatile int> y = std::move(x);
  return *y;
}

int test_volatile2() {
  std::unique_ptr<volatile int> x;
  return *x;
}

int test_volatile3() {
  std::shared_ptr<volatile int> x;
  std::shared_ptr<volatile int> y = std::move(x);
  return *y;
}

int test_volatile4() {
  std::shared_ptr<volatile int> x;
  return *x;
}

int test_const1() {
  std::unique_ptr<const int> x;
  std::unique_ptr<const int> y = std::move(x);
  return *y;
}

int test_const2() {
  std::unique_ptr<const int> x;
  return *x;
}

int test_const3() {
  std::shared_ptr<const int> x;
  std::shared_ptr<const int> y = std::move(x);
  return *y;
}

int test_const4() {
  std::shared_ptr<const int> x;
  return *x;
}
