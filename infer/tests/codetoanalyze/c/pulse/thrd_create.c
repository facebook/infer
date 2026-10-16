/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stddef.h>

// declarations from <threads.h>, which is not available on all platforms
typedef unsigned long thrd_t;
typedef int (*thrd_start_t)(void*);
int thrd_create(thrd_t* thr, thrd_start_t func, void* arg);

int deref_int_arg(void* arg) { return *(int*)arg; }

int thrd_create_null_arg_bad() {
  thrd_t thread;
  return thrd_create(&thread, deref_int_arg, NULL);
}

int thrd_create_ok() {
  thrd_t thread;
  int x = 0;
  return thrd_create(&thread, deref_int_arg, &x);
}
