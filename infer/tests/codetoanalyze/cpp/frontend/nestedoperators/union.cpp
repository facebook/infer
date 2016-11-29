/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
