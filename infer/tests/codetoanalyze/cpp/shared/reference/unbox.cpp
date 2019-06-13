/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int fun_p(int* p) { return *p; }
int fun_v(int p) { return p; }
int fun_r(int& p) { return p; }

// cfgs should look similar for unbox_ref and unbox_ptr

// test conversions from int& to {int*, int, int&)
void unbox_ref() {
  int a = 3;
  int& r = a;

  fun_p(&r);
  fun_v(r);
  fun_r(r);
}

// test conversions from int* to {int*, int, int&)
void unbox_ptr() {
  int a = 3;
  int* p = &a;

  fun_p(p);
  fun_v(*p);
  fun_r(*p);
}
