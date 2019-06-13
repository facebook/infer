/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

void decreasing_pointer_Good(struct S1* x) {
  int* p = &(x->f[1]);
  p--;
  *p = 0;
}

void decreasing_pointer_Bad(struct S1* x) {
  int* p = &(x->f[1]);
  p--;
  p--;
  *p = 0;
}
