/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
struct S1 {
  int f[2];
};

void array_field_access_Good(struct S1 x, struct S1 y) {
  int a[10];
  x.f[0] = 1;
  y.f[0] = 20;
  a[x.f[0]] = 0;
}

void array_field_access_Bad(struct S1 x, struct S1 y) {
  int a[10];
  y.f[0] = 20;
  x.f[0] = 1;
  a[y.f[0]] = 0;
}
