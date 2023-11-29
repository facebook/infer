/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <functional>
#include <memory>

int read_shared_ptr_param(std::shared_ptr<int> x) {
  int a;
  int b;
  return *x;
}

int multiple_read_shared_ptr_param(std::shared_ptr<int> x) {
  int a;
  int b = *x;
  int c;
  return *x;
}
