/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

int test_volatile1_bad() {
  std::unique_ptr<volatile int> x;
  std::unique_ptr<volatile int> y = std::move(x);
  return *y;
}

int test_volatile2_bad() {
  std::unique_ptr<volatile int> x;
  return *x;
}

int test_volatile3_bad() {
  std::shared_ptr<volatile int> x;
  std::shared_ptr<volatile int> y = std::move(x);
  return *y;
}

int test_volatile4_bad() {
  std::shared_ptr<volatile int> x;
  return *x;
}

int test_const1_bad() {
  std::unique_ptr<const int> x;
  std::unique_ptr<const int> y = std::move(x);
  return *y;
}

int test_const2_bad() {
  std::unique_ptr<const int> x;
  return *x;
}

int test_const3_bad() {
  std::shared_ptr<const int> x;
  std::shared_ptr<const int> y = std::move(x);
  return *y;
}

int test_const4_bad() {
  std::shared_ptr<const int> x;
  return *x;
}
