/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
namespace copy_array_field {
struct X {
  int* p;
  int x[10]; // array field
};

int npe() {
  X x1;
  x1.p = 0;
  X x2 = x1; // will call default copy constructor
  return *x2.p;
}

int no_npe() {
  int a = 0;
  X x1;
  x1.p = &a;
  X x2 = x1; // will call default copy constructor
  return *x2.p;
}
}
