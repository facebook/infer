/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdlib.h>

struct Person {
  int age;
  int height;
  int weight;
};

int simple_null_pointer() {
  struct Person* max = 0;
  return max->age;
}

struct Person* Person_create(int age, int height, int weight) {
  struct Person* who = 0;
  return who;
}

int get_age(struct Person* who) { return who->age; }

int null_pointer_interproc() {
  struct Person* joe = Person_create(32, 64, 140);
  return get_age(joe);
}

int negation_in_conditional() {
  int* x = 0;
  if (!x)
    return 0;
  else
    return *x; // this never happens
}

static int* foo() { return 0; }

void null_pointer_with_function_pointer() {
  int* (*fp)();
  fp = foo;
  int* x = fp();
  *x = 3;
}

void use_exit(struct Person* htbl) {
  if (!htbl)
    exit(0);
  int x = htbl->age;
}

void basic_null_dereference() {
  int* p = NULL;
  *p = 42; // NULL dereference
}

void no_check_for_null_after_malloc() {
  int* p;
  p = (int*)malloc(sizeof(int));
  *p = 42; // NULL dereference
  free(p);
}

void no_check_for_null_after_realloc() {
  int* p;
  p = (int*)malloc(sizeof(int) * 5);
  if (p) {
    p[3] = 42;
  }
  int* q = (int*)realloc(p, sizeof(int) * 10);
  if (!q)
    free(p);
  q[7] = 0; // NULL dereference
  free(q);
}

void assign(int* p, int n) { *p = n; }

void potentially_null_pointer_passed_as_argument() {
  int* p = NULL;
  p = (int*)malloc(sizeof(int));
  assign(p, 42); // NULL dereference
  free(p);
}

void null_passed_as_argument() {
  assign(NULL, 42); // NULL dereference
}

void allocated_pointer_passed_as_argument() {
  int* p = NULL;
  p = (int*)malloc(sizeof(int));
  if (p) {
    assign(p, 42);
    free(p);
  }
}

int* unsafe_allocate() {
  int* p = NULL;
  p = (int*)malloc(sizeof(int));
  return p;
}

int* safe_allocate() {
  int* p = NULL;
  while (!p) {
    p = (int*)malloc(sizeof(int));
  }
  return p;
}

void function_call_can_return_null_pointer() {
  int* p = NULL;
  p = unsafe_allocate();
  assign(p, 42); // NULL dereference
  free(p);
}

void function_call_returns_allocated_pointer() {
  int* p = NULL;
  p = safe_allocate();
  assign(p, 42);
  free(p);
}
