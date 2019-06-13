/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct {
  int a;
  int b;
} * x;

union {
  int e;
  int f;

  struct {
    int w;
    int u;
  } g;

  int h;
} y;

int main() {
  int l;

  x->a = 1;
  y.f = 7;
  y.g.u = y.f;

  y.g.w = x->b;
  return 0;
}
