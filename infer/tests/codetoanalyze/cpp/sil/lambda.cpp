/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void use_lambda() {
  auto f = [](int x) { return x + 1; };
  int y = f(42);
}

int use_capturing_lambda(int a) {
  auto f = [&a](int x) { return x + a; };
  return f(10);
}
