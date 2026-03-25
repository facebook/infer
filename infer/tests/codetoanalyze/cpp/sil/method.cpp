/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct Point {
  int x;
  int y;
  int sum() { return x + y; }
};

int use_method() {
  Point p;
  p.x = 1;
  p.y = 2;
  return p.sum();
}
