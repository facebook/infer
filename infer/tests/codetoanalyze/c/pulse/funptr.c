/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stddef.h>

void do_nothing(int** _ptr) { return; }

void assign_NULL(int** ptr) { *ptr = NULL; }

void call_funptr(void (*funptr)(int**), int** ptr) { (*funptr)(ptr); }

void call_call_funptr(void (*funptr)(int**), int** ptr) {
  call_funptr(funptr, ptr);
}

// Basic case: specialized in pre-analysis

void test_syntactic_specialization_bad(int* ptr) {
  call_call_funptr(&assign_NULL, &ptr);
  *ptr = 42; // NULL dereference here
}

// Specialize over returned funptr

void (*return_funptr(void))(int**) { return &assign_NULL; }

void test_returned_funptr_specialization_bad(int* ptr) {
  void (*funptr)(int**) = return_funptr();
  call_call_funptr(funptr, &ptr);
  *ptr = 42; // NULL dereference here
}

// Playing around with conditions

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

// Playing around with structures

typedef struct FunPtrCallback {
  void (*f)(int**);
} callback_s;

void set_callback(callback_s* callback, void (*f)(int**)) { callback->f = f; }
void apply_callback(callback_s* callback, int** ptr) { (*callback->f)(ptr); }

void test_assign_NULL_callback_bad(int* ptr) {
  callback_s callback = {.f = &assign_NULL};
  apply_callback(&callback, &ptr);
  *ptr = 42; // NULL dereference here
}

void test_do_nothing_callback_good(int* ptr) {
  callback_s callback = {.f = &do_nothing};
  apply_callback(&callback, &ptr);
  *ptr = 42; // latent NULL dereference here
}

void test_update_callback_bad(callback_s* callback, int* ptr) {
  set_callback(callback, &assign_NULL);
  apply_callback(callback, &ptr);
  *ptr = 42; // NULL dereference here
}

void test_update_callback_good(callback_s* callback, int* ptr) {
  set_callback(callback, &do_nothing);
  apply_callback(callback, &ptr);
  *ptr = 42; // latent NULL dereference here
}
