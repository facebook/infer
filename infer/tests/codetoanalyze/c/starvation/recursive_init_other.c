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
typedef union {
  char __size[4];
  int __align;
} pthread_mutexattr_t;
enum { PTHREAD_MUTEX_NORMAL, PTHREAD_MUTEX_RECURSIVE };
int pthread_mutex_init(pthread_mutex_t*, const pthread_mutexattr_t*);
int pthread_mutexattr_init(pthread_mutexattr_t*);
int pthread_mutexattr_settype(pthread_mutexattr_t*, int);

struct OtherCounter {
  pthread_mutex_t mutex;
  int count;
};

void other_counter_init(struct OtherCounter* c) {
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&c->mutex, &attr);
  c->count = 0;
}
