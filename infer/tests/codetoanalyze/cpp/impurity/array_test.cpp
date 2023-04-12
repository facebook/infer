/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
// modifies a & b
void array_mod_impure(int a[10], int b[10], int c) {
  a[0] = c;
  b[0] = c;
}

// modifies a twice
void array_mod_both_impure(int a[10], int b) {
  a[0] = b;
  a[1] = 0;
}

// modifies a
void call_array_mod_impure(int a[10]) {
  int b[10];
  array_mod_impure(a, b, 9);
}

void call_array_mod_with_fresh_pure() {
  int a[10];
  array_mod_impure(a, a, 0);
}
void call_array_mod_with_both_fresh_pure() {
  int a[10];
  int b[10];
  array_mod_impure(a, b, 0);
}

// modifies array
void alias_mod_impure(int array[], int i, int j) {
  int* a = array;
  a[j] = i;
}

struct Foo {
  int x;
};

void modify_direct_impure(Foo array[10], int i, Foo foo) { array[i].x = foo.x; }

void modify_ptr_impure(Foo array[10], Foo foo) {
  Foo* tmp = array;
  tmp->x = foo.x;
}

void call_impure_with_fresh_pure() {
  struct Foo f1 = {1};
  struct Foo f2 = {2};
  struct Foo f3 = {3};
  Foo array[2] = {f1, f2};
  modify_direct_impure(array, 0, f3);
  modify_ptr_impure(array, f3);
}

// Note: Unlike C++, in Java, this is gonna be impure because
// everything is passed by reference in Java
int modify_by_copy_pure(Foo array[1], int i, Foo foo) {
  Foo tmp = array[i];
  tmp.x = foo.x;
  return tmp.x;
}
