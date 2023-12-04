/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct s {
  int a;
  int b;
};

void struct_copy_Ok() {
  int a[5];
  struct s x, y;
  x.a = 3;
  x.b = 5;
  y = x;
  a[y.a] = 0;
}

void struct_copy_Bad() {
  int a[3];
  struct s x, y;
  x.a = 3;
  x.b = 5;
  y = x;
  a[y.b] = 0;
}

struct t {
  struct s s;
  int c;
};

void nested_struct_copy_Ok() {
  int a[7];
  struct t x, y;
  x.s.a = 3;
  x.s.b = 5;
  x.c = 7;
  y = x;
  a[y.s.a] = 0;
}

void nested_struct_copy_Bad() {
  int a[3];
  struct t x, y;
  x.s.a = 3;
  x.s.b = 5;
  x.c = 7;
  y = x;
  a[y.s.b] = 0;
}

struct s get_struct() {
  struct s x;
  x.a = 3;
  x.b = 5;
  return x;
}

void struct_copy_from_function_call_Ok() {
  int a[5];
  struct s x;
  x = get_struct();
  a[x.a] = 0;
}

void struct_copy_from_function_call_Bad_FN() {
  int a[3];
  struct s x;
  x = get_struct();
  a[x.b] = 0;
}

struct s get_struct_wrapper() {
  return get_struct();
}

void struct_copy_from_wrapper_call_Ok() {
  int a[5];
  struct s x;
  x = get_struct_wrapper();
  a[x.a] = 0;
}

void struct_copy_from_wrapper_call_Bad_FN() {
  int a[3];
  struct s x;
  x = get_struct_wrapper();
  a[x.b] = 0;
}

void struct_copy_decl_Ok() {
  int a[5];
  struct s x;
  x.a = 3;
  x.b = 5;
  struct s y = x;
  a[y.a] = 0;
}

void struct_copy_decl_Bad() {
  int a[3];
  struct s x;
  x.a = 3;
  x.b = 5;
  struct s y = x;
  a[y.b] = 0;
}

void struct_copy_decl_by_function_Ok() {
  int a[5];
  struct s x = get_struct();
  a[x.a] = 0;
}

void struct_copy_decl_by_function_Bad() {
  int a[3];
  struct s x = get_struct();
  a[x.b] = 0;
}
