/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

int array_init_bad() {
  int t[2][3][2] = {{{1, 1}, {2, 2}, {3, 3}}, {{4, 4}, {5, 5}, {1, 0}}};
  if (t[0][1][0] == 2 && t[1][2][1] == 0) {
    int* p = NULL;
    *p = 42;
  }
}

int array_init_ok() {
  int t[2][3][2] = {{{1, 1}, {2, 2}, {3, 3}}, {{4, 4}, {5, 5}, {1, 0}}};
  if (t[0][1][0] != 2 || t[1][2][1] != 0) {
    int* p = NULL;
    *p = 42;
  }
}

int array_zero_filler_bad() {
  int* t[4] = {0};
  return *t[3];
}

int array_empty_braces_bad() {
  int* t[4] = {};
  return *t[2];
}

int array_designated_filler_bad() {
  int* t[4] = {[1] = NULL};
  return *t[3];
}

struct pair {
  int* p;
  int x;
};

int array_struct_filler_bad() {
  struct pair t[3] = {{NULL, 1}};
  return *t[2].p;
}

struct table {
  int* entries[4];
};

int struct_array_member_filler_bad() {
  struct table t = {{NULL}};
  return *t.entries[2];
}

int array_filler_values_ok() {
  int t[4] = {1};
  if (t[0] != 1 || t[3] != 0) {
    int* p = NULL;
    *p = 42;
  }
}

int array_explicit_before_filler_ok() {
  int x = 0;
  int* t[4] = {&x};
  return *t[0];
}

// the remaining elements exceed --clang-compound-literal-init-limit, so the
// array is zero-initialized with a builtin that is not modelled
int FN_array_large_filler_bad() {
  int* t[16] = {0};
  return *t[10];
}
