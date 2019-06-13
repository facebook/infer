/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <Foundation/Foundation.h>

int define_array_ok() {
  size_t len = 10;
  uint8_t a[len]; // we should not report on array definition

  return 0;
}

int bad1() {
  int a[10];

  return a[2]; // report here as values of the array are not initialized
}

void use_int(int);

int bad2() {
  int a[10];
  use_int(a[2]); // report here as values of the array are not initialized

  return 0;
}

int use_array_ok() {
  int a[10];
  a[1] = 5;

  return a[1];
}

int initialize_array_ok1() {
  int arr[10];
  arr[0] = 5;
}

int initialize_array_ok2() {
  int arr[] = {1, 2};
  arr[0] = 5;
}

void use_array(int[]);

int pass_array_FN() {
  int arr[5];
  use_array(arr); // we do not report here because of intraprocedural analysi a
                  // the array passed by ref
}

int array_length_undef_bad() {
  int x;
  int arr[x];
}

init_array_helper(int*);

void pass_array_address_ok() {
  int x[8];
  init_array_helper(x);
}
