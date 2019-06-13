/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <pthread.h>

int normal_life_ok(pthread_mutex_t* m) {
  if (pthread_mutex_init(m, 0))
    return 0;
  if (pthread_mutex_lock(m))
    return 0;
  if (pthread_mutex_unlock(m))
    return 0;
  if (pthread_mutex_destroy(m))
    return 0;
  return 1;
}

int normal_ok2() {
  pthread_mutex_t m;
  normal_life_ok(&m);
}

void FN_double_lock_bad(pthread_mutex_t* m) {
  pthread_mutex_lock(m);
  pthread_mutex_lock(m);
}

void double_lock_uninit_bad() {
  pthread_mutex_t m;
  FN_double_lock_bad(&m);
}

void double_lock_bad2() {
  pthread_mutex_t m;
  pthread_mutex_init(&m, 0);
  FN_double_lock_bad(&m);
}

void double_unlock_bad(pthread_mutex_t* m) {
  pthread_mutex_unlock(m);
  pthread_mutex_unlock(m);
}

void double_unlock_bad2() {
  pthread_mutex_t m;
  pthread_mutex_init(&m, 0);
  pthread_mutex_lock(&m);
  double_unlock_bad(&m);
}

void double_init_bad(pthread_mutex_t* m) {
  pthread_mutex_init(m, 0);
  pthread_mutex_init(m, 0);
}

// Already reported in double_init_bad
void double_init_ok() {
  pthread_mutex_t m;
  double_init_bad(&m);
}
