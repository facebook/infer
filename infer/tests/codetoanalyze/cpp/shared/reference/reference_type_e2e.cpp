/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void zero_ptr(int* p) { *p = 0; }
void zero_ref(int& p) { p = 0; }

int ptr_div0() {
  int a = 2;
  int* p = &a;
  *p = 0;
  return 1 / a;
}

int ptr_div0_function() {
  int a = 2;
  zero_ptr(&a);
  return 1 / a;
}

int ptr_div0_function_temp_var() {
  int a = 2;
  int* r = &a;
  zero_ptr(r);
  return 1 / a;
}

int ref_div0() {
  int a = 2;
  int& r = a;
  r = 0;
  return 1 / a;
}

int ref_div0_function() {
  int a = 2;
  zero_ref(a);
  return 1 / a;
}

int ref_div0_function_temp_var() {
  int a = 2;
  int& r = a;
  zero_ref(r);
  return 1 / a;
}

int ref_div0_nested_assignment() {
  int a = 2;
  int b = 3;
  int& r1 = a;
  int& r2 = r1 = b = 1;
  r2 = 0;
  return 1 / a;
}

int ref_div1_nested_assignment() {
  int a = 2;
  int b = 3;
  int& r1 = a;
  int& r2 = r1 = b = 1;
  r2 = 0;
  return 1 / b;
}
