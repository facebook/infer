/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct FakeMut {
  int blah;
};
void pthread_mutex_lock(struct FakeMut*);
void pthread_mutex_lock1(struct FakeMut*);
void pthread_mutex_unlock(struct FakeMut*);
void* malloc(unsigned long);

struct FakeMut global_m1;

struct MutexWrapper {
  struct FakeMut* mutex;
};

struct Wrapper {
  struct FakeMut* m1;
  struct MutexWrapper m2;
  void (*fn1)(struct Wrapper*);
};

struct SomethingElse {
  int foo;
};

struct SomethingElse some_global;

void mutex_wrapper_lock(struct MutexWrapper w) { pthread_mutex_lock(w.mutex); }

void lock_m2_and_do_soemthing_else(struct SomethingElse* other) {
  pthread_mutex_lock(&global_m1);
}

void wrap_lock_direct(struct FakeMut* m) { pthread_mutex_lock(m); }

void wrap_locking_m2(struct Wrapper* str) { mutex_wrapper_lock(str->m2); }

int m1_then_m2(struct Wrapper* s) {
  wrap_lock_direct(s->m1);
  struct Wrapper s1 = *s;
  (*s1.fn1)(s);
  struct SomethingElse* other = malloc(100);
  lock_m2_and_do_soemthing_else(other);
  pthread_mutex_unlock(&global_m1);
  pthread_mutex_unlock(s->m2.mutex);
  pthread_mutex_unlock(s->m1);
  return 0;
}

int simple_locking(struct Wrapper* s) {
  pthread_mutex_lock(&global_m1);
  pthread_mutex_lock(s->m2.mutex);
  pthread_mutex_lock(s->m1);
  pthread_mutex_unlock(s->m1);
  pthread_mutex_unlock(s->m2.mutex);
  pthread_mutex_unlock(&global_m1);
  return 1;
}
