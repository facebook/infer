/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct FakeMut {
  int blah;
};
void pthread_mutex_lock(struct FakeMut*);
void pthread_mutex_unlock(struct FakeMut*);

struct FakeMut m1_fptr;
struct FakeMut m2_fptr;

void lock_m2_fptr_indirectly() { pthread_mutex_lock(&m2_fptr); }

void (*LOCK_M2_INDIRECTLY)(void) = &lock_m2_fptr_indirectly;

int lock_m2_fptr_m1_fptr_function_pointer() {
  pthread_mutex_lock(&m1_fptr);
  (*LOCK_M2_INDIRECTLY)();
  pthread_mutex_unlock(&m2_fptr);
  pthread_mutex_unlock(&m1_fptr);
  return 0;
}

int lock_m2_fptr_m1_fptr_no_function_pointer() {
  pthread_mutex_lock(&m2_fptr);
  pthread_mutex_lock(&m1_fptr);
  pthread_mutex_unlock(&m1_fptr);
  pthread_mutex_unlock(&m2_fptr);
  return 1;
}
