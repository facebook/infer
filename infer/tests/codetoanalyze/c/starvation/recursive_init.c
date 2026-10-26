/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// glibc's layout
typedef union {
  struct __pthread_mutex_s {
    int __lock;
    unsigned int __count;
    int __owner;
    unsigned int __nusers;
    int __kind;
  } __data;
  char __size[40];
  long __align;
} pthread_mutex_t;
typedef union {
  char __size[4];
  int __align;
} pthread_mutexattr_t;
enum {
  PTHREAD_MUTEX_NORMAL,
  PTHREAD_MUTEX_RECURSIVE,
  PTHREAD_MUTEX_ERRORCHECK
};
int pthread_mutex_init(pthread_mutex_t*, const pthread_mutexattr_t*);
int pthread_mutex_lock(pthread_mutex_t*);
int pthread_mutex_unlock(pthread_mutex_t*);
int pthread_mutexattr_init(pthread_mutexattr_t*);
int pthread_mutexattr_settype(pthread_mutexattr_t*, int);

// PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
static pthread_mutex_t initializer_mutex = {
    {0, 0, 0, 0, PTHREAD_MUTEX_RECURSIVE}};

void relock_initializer_mutex_ok() {
  pthread_mutex_lock(&initializer_mutex);
  pthread_mutex_lock(&initializer_mutex);
  pthread_mutex_unlock(&initializer_mutex);
  pthread_mutex_unlock(&initializer_mutex);
}

// PTHREAD_MUTEX_INITIALIZER
static pthread_mutex_t plain_mutex = {{0, 0, 0, 0, PTHREAD_MUTEX_NORMAL}};

void relock_plain_mutex_bad() {
  pthread_mutex_lock(&plain_mutex);
  pthread_mutex_lock(&plain_mutex);
  pthread_mutex_unlock(&plain_mutex);
  pthread_mutex_unlock(&plain_mutex);
}

static pthread_mutex_t attr_mutex;

void init_attr_mutex() {
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&attr_mutex, &attr);
}

void relock_attr_mutex_ok() {
  pthread_mutex_lock(&attr_mutex);
  pthread_mutex_lock(&attr_mutex);
  pthread_mutex_unlock(&attr_mutex);
  pthread_mutex_unlock(&attr_mutex);
}

static pthread_mutex_t errorcheck_mutex;

void init_errorcheck_mutex() {
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
  pthread_mutex_init(&errorcheck_mutex, &attr);
}

void relock_errorcheck_mutex_bad() {
  pthread_mutex_lock(&errorcheck_mutex);
  pthread_mutex_lock(&errorcheck_mutex);
  pthread_mutex_unlock(&errorcheck_mutex);
  pthread_mutex_unlock(&errorcheck_mutex);
}

struct Counter {
  pthread_mutex_t mutex;
  int count;
};

void counter_init(struct Counter* c) {
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(&c->mutex, &attr);
  c->count = 0;
}

void counter_increment(struct Counter* c) {
  pthread_mutex_lock(&c->mutex);
  c->count++;
  pthread_mutex_unlock(&c->mutex);
}

void counter_call_under_lock_ok(struct Counter* c) {
  pthread_mutex_lock(&c->mutex);
  counter_increment(c);
  pthread_mutex_unlock(&c->mutex);
}

void init_recursive_mutex(pthread_mutex_t* mutex) {
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(mutex, &attr);
}

static pthread_mutex_t helper_mutex;

void init_helper_mutex() { init_recursive_mutex(&helper_mutex); }

void relock_helper_mutex_ok() {
  pthread_mutex_lock(&helper_mutex);
  pthread_mutex_lock(&helper_mutex);
  pthread_mutex_unlock(&helper_mutex);
  pthread_mutex_unlock(&helper_mutex);
}

static pthread_mutex_t alias_mutex;

void init_alias_mutex() {
  pthread_mutex_t* mutex = &alias_mutex;
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(mutex, &attr);
}

void relock_alias_mutex_ok() {
  pthread_mutex_lock(&alias_mutex);
  pthread_mutex_lock(&alias_mutex);
  pthread_mutex_unlock(&alias_mutex);
  pthread_mutex_unlock(&alias_mutex);
}

struct AliasCounter {
  pthread_mutex_t mutex;
  int count;
};

void alias_counter_init(struct AliasCounter* c) {
  pthread_mutex_t* mutex = &c->mutex;
  pthread_mutexattr_t attr;
  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
  pthread_mutex_init(mutex, &attr);
  c->count = 0;
}

void alias_counter_increment(struct AliasCounter* c) {
  pthread_mutex_lock(&c->mutex);
  c->count++;
  pthread_mutex_unlock(&c->mutex);
}

void alias_counter_call_under_lock_ok(struct AliasCounter* c) {
  pthread_mutex_lock(&c->mutex);
  alias_counter_increment(c);
  pthread_mutex_unlock(&c->mutex);
}

// the frontend drops the initializers of static locals
void FP_relock_static_local_mutex_ok() {
  static pthread_mutex_t mutex = {{0, 0, 0, 0, PTHREAD_MUTEX_RECURSIVE}};
  pthread_mutex_lock(&mutex);
  pthread_mutex_lock(&mutex);
  pthread_mutex_unlock(&mutex);
  pthread_mutex_unlock(&mutex);
}

void make_recursive_attr(pthread_mutexattr_t* attr) {
  pthread_mutexattr_init(attr);
  pthread_mutexattr_settype(attr, PTHREAD_MUTEX_RECURSIVE);
}

static pthread_mutex_t attr_helper_mutex;

void init_attr_helper_mutex() {
  pthread_mutexattr_t attr;
  make_recursive_attr(&attr);
  pthread_mutex_init(&attr_helper_mutex, &attr);
}

// the attribute is made recursive in another function, which is not tracked
void FP_relock_attr_helper_mutex_ok() {
  pthread_mutex_lock(&attr_helper_mutex);
  pthread_mutex_lock(&attr_helper_mutex);
  pthread_mutex_unlock(&attr_helper_mutex);
  pthread_mutex_unlock(&attr_helper_mutex);
}

// initialised in recursive_init_other.c
struct OtherCounter {
  pthread_mutex_t mutex;
  int count;
};

void other_counter_increment(struct OtherCounter* c) {
  pthread_mutex_lock(&c->mutex);
  c->count++;
  pthread_mutex_unlock(&c->mutex);
}

// initialisations are only looked for in the file of the relock
void FP_other_counter_call_under_lock_ok(struct OtherCounter* c) {
  pthread_mutex_lock(&c->mutex);
  other_counter_increment(c);
  pthread_mutex_unlock(&c->mutex);
}
