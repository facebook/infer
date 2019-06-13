/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Person {
 public:
  Person(int i) { x = i; }
  Person() {}
  int x;
};

int array_of_person() {
  Person arr[10] = {Person(), Person(), Person()};
  return (arr[0]).x;
}

int matrix_of_person() {
  Person arr[2][2] = {Person(), Person(), Person(), Person()};
  return (arr[0][1]).x;
}

struct Z {
  int a;
  int b;
};

void initialization_c_style() {
  struct Z z[2] = {{1, 2}, {2, 3}};
  struct Z z2;
}

// Our handling assumes that either all the array elements are initialised
// with a constructor or not, so this doesn't work.
void initialization_mixed_styles_not_handled_correctly() {
  struct Z old;
  struct Z z[2] = {{1, 2}, old};
  struct Z z2;
}
