/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef union {
  char __size[40];
  long __align;
} pthread_mutex_t;
int pthread_mutex_lock(pthread_mutex_t*);
int pthread_mutex_unlock(pthread_mutex_t*);

static pthread_mutex_t lock;

void static_lock_2_work(void) {
  pthread_mutex_lock(&lock);
  pthread_mutex_unlock(&lock);
}

void static_lock_2_relock_bad(void) {
  pthread_mutex_lock(&lock);
  static_lock_2_work();
  pthread_mutex_unlock(&lock);
}
