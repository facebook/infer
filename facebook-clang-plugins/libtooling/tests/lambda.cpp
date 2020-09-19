/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
int main() {
  int m, n;

  auto f = []() { return 1; };

  auto bar = [&m, n](int a) { return m; };

  auto init_capture = [i = 0]() { return i; };

  return 0;
}
