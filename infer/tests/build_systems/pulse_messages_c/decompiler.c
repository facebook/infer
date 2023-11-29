/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

struct s;

struct a {
  struct s*** a_ptr;
};

struct b {
  struct a a;
};

struct s {
  int* ptr;
  struct b next;
};

void trace_name_parameter(int *p) {
  p = NULL;
  *p = 42;
}

void trace_name_local() {
  int* p = NULL;
  *p = 42;
}

void trace_name_local_aliased() {
  int b = random();
  int x = 0;
  int* p;
  int y = 0;
  if (b == 0) {
    p = (int *)b;
  }
  *p = 42;
}

int* trace_inner() {
  int* p = NULL;
  int* q = p;
  return q;
}

void trace_outer_name_return() {
  struct s x;
  x.ptr = trace_inner();
  *x.ptr = 42;
}

void trace_outer_name_return_complex() {
  int* y;
  struct s* x = (struct s*) malloc(sizeof(struct s));
  struct s a;
  if (x) {
    a.ptr = trace_inner();
    y = a.ptr;
    x->ptr = y;
    *x->ptr = 42;
    y = y;
  }
  free(x);
}

void deref_both(int* x, int* y) {
  *x = 42;
  *y = 52;
}

void trace_outer_alias_return_indirect_deref_complex() {
  int* y;
  struct s* x = (struct s*) malloc(sizeof(struct s));
  struct s a;
  if (x) {
    a.ptr = trace_inner();
    y = a.ptr;
    x->ptr = y;
    int z;
    int* p = &z;
    deref_both((p + 0), x->ptr);
    y = y;
  }
  free(x);
}

void trace_outer_anon_return() {
  *(trace_inner()) = 42;
}

void fields_in_correct_order(struct s* x) {
  (**x->next.a.a_ptr)->ptr = NULL;
  *(**x->next.a.a_ptr)->ptr = 42;
}


void array_deref_of_address() {
  int* x = NULL;
  *(&x[0]) = 42;
}

void pass_null_to_deref() {
  int z = 11;
  deref_both(NULL, &z);
}
