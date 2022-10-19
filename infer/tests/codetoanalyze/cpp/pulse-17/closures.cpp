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
