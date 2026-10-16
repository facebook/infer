/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// bionic's layout and initializers
typedef struct {
  int __private[10];
} pthread_mutex_t;
enum { PTHREAD_MUTEX_NORMAL, PTHREAD_MUTEX_RECURSIVE };
int pthread_mutex_lock(pthread_mutex_t*);
int pthread_mutex_unlock(pthread_mutex_t*);

// PTHREAD_RECURSIVE_MUTEX_INITIALIZER
static pthread_mutex_t bionic_initializer_mutex = {
    {((PTHREAD_MUTEX_RECURSIVE & 3) << 14)}};

void bionic_relock_initializer_mutex_ok() {
  pthread_mutex_lock(&bionic_initializer_mutex);
  pthread_mutex_lock(&bionic_initializer_mutex);
  pthread_mutex_unlock(&bionic_initializer_mutex);
  pthread_mutex_unlock(&bionic_initializer_mutex);
}

// PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t bionic_plain_mutex = {
    {((PTHREAD_MUTEX_NORMAL & 3) << 14)}};

void bionic_relock_plain_mutex_bad() {
  pthread_mutex_lock(&bionic_plain_mutex);
  pthread_mutex_lock(&bionic_plain_mutex);
  pthread_mutex_unlock(&bionic_plain_mutex);
  pthread_mutex_unlock(&bionic_plain_mutex);
}
