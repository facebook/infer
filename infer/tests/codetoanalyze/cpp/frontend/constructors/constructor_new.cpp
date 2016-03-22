/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class Person {
 public:
  Person(int i) { x = i; }

  Person(int i, int j, int k) {
    x = i;
    y = j;
    z = k;
  }
  int x;
  int y;
  int z;
};

int getValue(int x) { return x; }

int constructor_1_arg_new_div0() {
  Person* p = new Person(5);
  return 1 / (p->x - 5);
}

int constructor_3_args_new_div0() {
  Person* p = new Person(5, 6, 7);
  return 1 / (p->z - 7);
}

int int_init_number() {
  int* x1 = new int(5);
  return 1 / (*x1 - 5);
}

float float_init_number() {
  float* x1 = new float(5.4);
  return 1 / (*x1 - 5.4);
}

int int_init_empty() {
  int* x1 = new int();
  return 1 / *x1;
}

int int_init_empty_list() {
  int x1{};
  return 1 / x1;
}

int int_init_empty_list_new() {
  int* x1 = new int{};
  return 1 / *x1;
}

int int_init_nodes() {
  int z = 6;
  int* y = new int(getValue(4));
  int* x = new int(getValue(0) ? getValue(1) : 1 + *y);
  return 1 / (*x - 5);
}

int constructor_nodes() {
  int z = 6;
  Person* p = new Person(getValue(0) ? getValue(1) : 1 + z);
  return 1 / (p->x - 7);
}
