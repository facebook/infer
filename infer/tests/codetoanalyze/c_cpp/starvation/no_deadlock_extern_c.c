/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <pthread.h>
extern void lock_m1_in_cpp();
extern void unlock_m1_in_cpp();
struct MyMutex {
  pthread_mutex_t* mutex;
};

struct MyMutex* extern_m1;

void lock_m1_in_c() { pthread_mutex_lock(extern_m1->mutex); }

/* The bellow doesn't deadlock. But infer
 * might think m1 is different when in C or when in C++ file
 * In that case it would report a deadlock*/
void lock_c_then_cpp() {
  lock_m1_in_c();
  lock_m1_in_cpp();
  unlock_m1_in_cpp();
  pthread_mutex_unlock(extern_m1->mutex);
}

void lock_cpp_then_c() {
  lock_m1_in_cpp();
  lock_m1_in_c();
  pthread_mutex_unlock(extern_m1->mutex);
  unlock_m1_in_cpp();
}
int main() { return 0; }
