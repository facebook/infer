/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void set_null(int** p) { *p = nullptr; }

void do_nothing(int** p) {}

void apply(void (*f)(int**), int** p) { (*f)(p); }

int test_funptr_call() {
  int x = 1;
  int* p = &x;
  apply(&set_null, &p);
  return *p;
}

int test_funptr_store() {
  int x = 1;
  int* p = &x;
  void (*f)(int**) = &set_null;
  (*f)(&p);
  return *p;
}
