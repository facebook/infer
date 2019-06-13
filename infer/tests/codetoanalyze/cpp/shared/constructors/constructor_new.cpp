/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace constructor_new {

class Person {
 public:
  Person() { x = 0; }
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

int int_array() {
  int* x2 = new int[getValue(5) ? getValue(5) : 3];
  x2[0] = 1;
  x2[1] = 2;
  return 1 / ((x2[0] + x2[1]) - 3);
}

int int_array_init() {
  int* arr = new int[100]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  return 1 / ((arr[0] + arr[1] + arr[2] + arr[3] + arr[4]) - 15);
}

// cfg ok, but size not known at frontend time, so no initialization
void array_of_class_with_not_constant_size() {
  Person* tarray = new Person[getValue(5) == 5 ? 5 : 3];
}

// constructor called for tarray[0]..tarray[9]
void array_of_person_with_constant_size() { Person* tarray = new Person[10]; }

// Also works fine for multidimendional arrays
void matrix_of_person() {
  Person** tarray = new Person*[10];
  tarray[0] = new Person[10];
}
} // namespace constructor_new
