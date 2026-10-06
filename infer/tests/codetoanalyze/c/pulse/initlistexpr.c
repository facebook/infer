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

typedef int int4 __attribute__((vector_size(16)));

struct vec_and_int {
  int4 v;
  int n;
};

void vector_init_bad() {
  int4 v = {1, 2, 3, 4};
  if (v[0] == 1 && v[3] == 4) {
    int* p = NULL;
    *p = 42;
  }
}

void vector_init_ok() {
  int4 v = {1, 2, 3, 4};
  if (v[0] != 1 || v[3] != 4) {
    int* p = NULL;
    *p = 42;
  }
}

void vector_compound_literal_bad() {
  if (((int4){1, 2, 3, 4})[2] == 3) {
    int* p = NULL;
    *p = 42;
  }
}

void vector_compound_literal_ok() {
  if (((int4){1, 2, 3, 4})[2] != 3) {
    int* p = NULL;
    *p = 42;
  }
}

void vector_in_struct_init_bad() {
  struct vec_and_int s = {{1, 2, 3, 4}, 5};
  if (s.v[1] == 2 && s.n == 5) {
    int* p = NULL;
    *p = 42;
  }
}

void vector_in_struct_init_ok() {
  struct vec_and_int s = {{1, 2, 3, 4}, 5};
  if (s.v[1] != 2 || s.n != 5) {
    int* p = NULL;
    *p = 42;
  }
}

void vector_init_use_after_free_bad() {
  int* q = (int*)malloc(sizeof(int));
  if (q == NULL) {
    return;
  }
  *q = 0;
  int4 v = {(free(q), 1), 2, 3, 4};
  *q = v[0];
}

// elements without an initializer are zero but are not modeled
void FP_vector_partial_init_ok() {
  int4 v = {1};
  if (v[1] != 0) {
    int* p = NULL;
    *p = 42;
  }
}

// a whole-vector copy, `v = w` or `v = {w}`, loses the values of the elements
void FP_vector_copy_then_check_ok() {
  int4 w = {1, 2, 3, 4};
  int4 v = w;
  if (v[0] != 1) {
    int* p = NULL;
    *p = 42;
  }
}

void FP_vector_braced_copy_then_check_ok() {
  int4 w = {1, 2, 3, 4};
  int4 v = {w};
  if (v[0] != 1) {
    int* p = NULL;
    *p = 42;
  }
}

void bitint_init_bad() {
  _BitInt(37) x = {5};
  if (x == 5) {
    int* p = NULL;
    *p = 42;
  }
}

void bitint_init_ok() {
  _BitInt(37) x = {5};
  if (x != 5) {
    int* p = NULL;
    *p = 42;
  }
}

// the unbraced branch creates no node, leaving a node without successors
int vector_init_dead_end(int c) {
  int4 v = {1, 2, 3, 4};
  if (c)
    (void)v;
  return 0;
}

void call_vector_init_dead_end_bad() {
  vector_init_dead_end(1);
  int* p = NULL;
  *p = 42;
}

// the parts of a complex number initialized from two values are not modeled
void FP_complex_init_ok() {
  _Complex double z = {1.0, 2.0};
  if (__real__ z != 1.0) {
    int* p = NULL;
    *p = 42;
  }
}
