/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
} // namespace copy_array_field
