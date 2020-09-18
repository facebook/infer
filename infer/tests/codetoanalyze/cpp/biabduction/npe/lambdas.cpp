/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int capture_by_ref_ok(int s) {
  auto f = [&]() { return s; };
  int x = f();
  int y = f();
  return x + y;
}
