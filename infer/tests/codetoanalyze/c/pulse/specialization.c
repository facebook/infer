/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

int invoke(int (*f)(int), int i) { return (*f)(i); }

int id(int i) { return i; }

int add_one(int i) { return invoke(id, i) + 1; }

int add_two(int i) {
  int one = invoke(id, 1);
  add_one(0);
  return invoke(add_one, i) + 1;
}

// recursion cycle
int add_more_bad(int i) {
  if (i > 0) {
    return invoke(add_more_bad, i - 1) + 1;
  }
  return 0;
}

void test_invoke_ok() {
  if (add_one(0) != 1) {
    int* p = NULL;
    *p = 42;
  }
}

void test_invoke_bad() {
  if (add_one(0) == 1) {
    int* p = NULL;
    *p = 42;
  }
}

void test_recursive_invoke_ok() {
  if (add_two(0) + add_one(0) != 3) {
    int* p = NULL;
    *p = 42;
  }
}

void test_recursive_invoke_bad() {
  if (add_two(0) + add_one(0) == 3) {
    int* p = NULL;
    *p = 42;
  }
}

// recursion cycle involving alias specialization
int two_pointers_recursion_bad(int* x, int* y, int i) {
  if (i > 0) {
    return *x + *y + two_pointers_recursion_bad(x, x, i - 1);
  }
  return 0;
}

void alias_recursion(int* z) { two_pointers_recursion_bad(z, z, 10); }

// recursion cycle involving closure specialization
void invoke_itself_bad(int (*f)(int), int i) {
  if (i > 0) {
    (*f)(i);
    invoke_itself_bad(f, i - 1);
  }
}

void specialize_invoke_itself_ok() { invoke_itself_bad(id, 10); }
