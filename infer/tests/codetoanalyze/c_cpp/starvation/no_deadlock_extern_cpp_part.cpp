/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <pthread.h>

struct MyMutex {
  pthread_mutex_t* mutex;
};

extern struct MyMutex* extern_m1;
extern "C" {
void lock_m1_in_cpp();
void unlock_m1_in_cpp();
}

void lock_m1_in_cpp() { pthread_mutex_lock(extern_m1->mutex); }

void unlock_m1_in_cpp() { pthread_mutex_unlock(extern_m1->mutex); }
