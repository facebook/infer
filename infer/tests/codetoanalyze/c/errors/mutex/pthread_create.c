/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <pthread.h>

void dummy() {}

void deref_pointer(int* x) { int y = *x; }

int pthread_create_dummy_ok() {
  pthread_t thread;
  return pthread_create(&thread, NULL, dummy, NULL);
}

int pthread_create_deref_bad() {
  pthread_t thread;
  return pthread_create(&thread, NULL, deref_pointer, NULL);
}

int pthread_create_deref_ok() {
  pthread_t thread;
  int x;
  return pthread_create(&thread, NULL, deref_pointer, &x);
}

extern void some_unknown_function(void);

int pthread_unknown_ok() {
  pthread_t thread;
  return pthread_create(&thread, NULL, some_unknown_function, NULL);
}
