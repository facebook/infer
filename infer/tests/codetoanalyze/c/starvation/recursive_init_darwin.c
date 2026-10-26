/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Darwin's layout and constants
typedef struct _opaque_pthread_mutex_t {
  long __sig;
  char __opaque[56];
} pthread_mutex_t;
typedef struct _opaque_pthread_mutexattr_t {
  long __sig;
  char __opaque[8];
} pthread_mutexattr_t;
#define PTHREAD_MUTEX_ERRORCHECK 1
#define PTHREAD_MUTEX_RECURSIVE 2
int pthread_mutex_init(pthread_mutex_t*, const pthread_mutexattr_t*);
int pthread_mutex_lock(pthread_mutex_t*);
int pthread_mutex_unlock(pthread_mutex_t*);
int pthread_mutexattr_init(pthread_mutexattr_t*);
int pthread_mutexattr_settype(pthread_mutexattr_t*, int);

// PTHREAD_RECURSIVE_MUTEX_INITIALIZER
static pthread_mutex_t darwin_initializer_mutex = {0x32AAABA2, {0}};

void darwin_relock_initializer_mutex_ok() {
  pthread_mutex_lock(&darwin_initializer_mutex);
  pthread_mutex_lock(&darwin_initializer_mutex);
  pthread_mutex_unlock(&darwin_initializer_mutex);
  pthread_mutex_unlock(&darwin_initializer_mutex);
}

// PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t darwin_plain_mutex = {0x32AAABA7, {0}};

void darwin_relock_plain_mutex_bad() {
  pthread_mutex_lock(&darwin_plain_mutex);
  pthread_mutex_lock(&darwin_plain_mutex);
  pthread_mutex_unlock(&darwin_plain_mutex);
  pthread_mutex_unlock(&darwin_plain_mutex);
}

static pthread_mutex_t darwin_attr_mutex;
static pthread_mutex_t darwin_errorcheck_mutex;

void darwin_init_mutexes() {
  pthread_mutexattr_t recursive, errorcheck;
  pthread_mutexattr_init(&recursive);
  pthread_mutexattr_settype(&recursive, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&darwin_attr_mutex, &recursive);
  pthread_mutexattr_init(&errorcheck);
  pthread_mutexattr_settype(&errorcheck, PTHREAD_MUTEX_ERRORCHECK);
  pthread_mutex_init(&darwin_errorcheck_mutex, &errorcheck);
}

void darwin_relock_attr_mutex_ok() {
  pthread_mutex_lock(&darwin_attr_mutex);
  pthread_mutex_lock(&darwin_attr_mutex);
  pthread_mutex_unlock(&darwin_attr_mutex);
  pthread_mutex_unlock(&darwin_attr_mutex);
}

void darwin_relock_errorcheck_mutex_bad() {
  pthread_mutex_lock(&darwin_errorcheck_mutex);
  pthread_mutex_lock(&darwin_errorcheck_mutex);
  pthread_mutex_unlock(&darwin_errorcheck_mutex);
  pthread_mutex_unlock(&darwin_errorcheck_mutex);
}
