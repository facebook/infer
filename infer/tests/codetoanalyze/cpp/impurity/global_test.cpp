/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
static int x;
int a[3];

void modify_global_primitive_impure() { x = 10; }

void modify_global_array_impure() { a[0] = 0; }

void call_modify_global() {
  modify_global_primitive_impure();
  modify_global_array_impure();
}

// modifies local static arr
int* local_static_var_impure() {

  // Lifetime of a static variable is throughout the program.
  static int arr[100];

  /* Some operations on arr[] */
  arr[0] = 10;
  arr[1] = 20;

  return arr;
}

void modify_global_inside_lamda_impure() {
  auto lam = [&]() { x++; };
}
