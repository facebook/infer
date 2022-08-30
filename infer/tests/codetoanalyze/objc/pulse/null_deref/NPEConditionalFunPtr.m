/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

void do_nothing(int**) { return; }

void assign_NULL(int** ptr) { *ptr = NULL; }

int funptr_if_bad() {
  int x = 0;
  int* ptr = &x;
  void (*funptr)(int**);
  if (!x) {
    funptr = &assign_NULL;
  } else {
    funptr = &do_nothing;
  }
  (*funptr)(&ptr); // Calling funptr assigned in if branch
  return *ptr;
}

int funptr_if_good() {
  int x = 0;
  int* ptr = &x;
  void (*funptr)(int**);
  if (!x) {
    funptr = &do_nothing;
  } else {
    funptr = &assign_NULL;
  }
  (*funptr)(&ptr); // Calling funptr assigned in if branch
  return *ptr;
}

int funptr_else_bad() {
  int x = 0;
  int* ptr = &x;
  void (*funptr)(int**);
  if (x) {
    funptr = &do_nothing;
  } else {
    funptr = &assign_NULL;
  }
  (*funptr)(&ptr); // Calling funptr assigned in else branch
  return *ptr;
}

int funptr_else_good() {
  int x = 0;
  int* ptr = &x;
  void (*funptr)(int**);
  if (x) {
    funptr = &assign_NULL;
  } else {
    funptr = &do_nothing;
  }
  (*funptr)(&ptr); // Calling funptr assigned in else branch
  return *ptr;
}

int funptr_conditional_call(int x) {
  int* ptr = &x;
  void (*funptr)(int**);
  if (!x) {
    funptr = &assign_NULL;
  } else {
    funptr = &do_nothing;
  }
  (*funptr)(&ptr);
  return *ptr;
}

int funptr_conditional_call_bad() { return funptr_conditional_call(0); }

int funptr_conditional_call_good() { return funptr_conditional_call(1); }

void apply_funptr_with_intptrptr(void (*funptr)(int**), int** ptr) {
  (*funptr)(ptr);
}

int funptr_apply_funptr_with_intptrptr_specialized_bad() {
  int x = 0;
  int* ptr = &x;
  void (*funptr)(int**);
  if (!x) {
    funptr = &assign_NULL;
  } else {
    funptr = &do_nothing;
  }
  apply_funptr_with_intptrptr(funptr,
                              &ptr); // Calling funptr assigned in if branch
  return *ptr;
}

int funptr_apply_funptr_with_intptrptr_specialized_good() {
  int x = 0;
  int* ptr = &x;
  void (*funptr)(int**);
  if (!x) {
    funptr = &do_nothing;
  } else {
    funptr = &assign_NULL;
  }
  apply_funptr_with_intptrptr(funptr,
                              &ptr); // Calling funptr assigned in if branch
  return *ptr;
}

void apply_funptr_with_intptrptr_and_after(void (*funptr)(int**),
                                           void (*after)(int**),
                                           int** ptr) {
  int x = 0;
  if (x) {
    funptr = &do_nothing;
  }
  apply_funptr_with_intptrptr(funptr, ptr);
  (*after)(ptr);
}

void dereference_dereference_ptr(int** ptr) { int x = **ptr; }

int funptr_apply_funptr_with_intptrptr_and_after_specialized_bad() {
  int x = 0;
  int* ptr = &x;
  void (*funptr)(int**);
  void (*after)(int**);
  if (!x) {
    funptr = &assign_NULL;
    after = &dereference_dereference_ptr;
  } else {
    funptr = &do_nothing;
    after = &do_nothing;
  }
  apply_funptr_with_intptrptr_and_after(
      funptr,
      after,
      &ptr); // Calling funptr assigned in if branch. NPE when calling after
  return *ptr;
}

int funptr_apply_funptr_with_intptrptr_and_after_respecialized_bad() {
  int x = 0;
  int* ptr = &x;
  void (*funptr)(int**);
  if (!x) {
    funptr = &assign_NULL;
  } else {
    funptr = &do_nothing;
  }
  apply_funptr_with_intptrptr_and_after(
      funptr,
      &dereference_dereference_ptr,
      &ptr); // Calling funptr assigned in if branch
  return *ptr;
}

void conditionnaly_apply_funptr_with_intptrptr(int x,
                                               int** ptr,
                                               void (*funptr)(int** ptr)) {
  if (x) {
    (*funptr)(ptr);
  }
  *ptr = NULL;
}

int funptr_conditionnaly_apply_funptr_with_intptrptr_unspecialized_bad() {
  int x = 0;
  int* ptr = NULL;
  void (*funptr)(int**);
  if (!x) {
    funptr = &assign_NULL;
  } else {
    funptr = &do_nothing;
  }
  conditionnaly_apply_funptr_with_intptrptr(
      x, &ptr, funptr); // funptr is not called; function is not specialized
  return *ptr;
}

int funptr_conditionnaly_apply_funptr_with_intptrptr_specialized_bad() {
  int x = 1;
  int* ptr = NULL;
  void (*funptr)(int**);
  if (!x) {
    funptr = &assign_NULL;
  } else {
    funptr = &do_nothing;
  }
  conditionnaly_apply_funptr_with_intptrptr(
      x, &ptr, funptr); // Calling funptr assigned in else branch
  return *ptr;
}
