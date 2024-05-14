/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct AMutex {
  int blah;
};
void pthread_mutex_lock(struct AMutex*);
void pthread_mutex_lock1(struct AMutex*);
void pthread_mutex_unlock(struct AMutex*);

struct AMutex m1;
struct AMutex m2;

void lock_m1_wrap() { pthread_mutex_lock(&m1); }
void unlock_m1_wrap() { pthread_mutex_unlock(&m1); }

// This should deadlock with direct_m2_m1(), but the analysis needs to
// see lock_m1() acquires the lock and apply that interprocedurally
int simple_unbalanced_lock() {
  lock_m1_wrap();
  pthread_mutex_lock(&m2);
  pthread_mutex_unlock(&m2);
  pthread_mutex_unlock(&m1);
  return 0;
}

int balanced_m2_m1_lock() {
  pthread_mutex_lock(&m2);
  pthread_mutex_lock(&m1);
  pthread_mutex_unlock(&m1);
  pthread_mutex_unlock(&m2);
  return 1;
}

// This shouldn't deadlock, because m1 and m2 are never held togehter
// But the analysis must see unlock_m1 unlocks m1, otherwise it would
// report deadlock
int indirect_unlock_m1() {
  pthread_mutex_lock(&m1);
  unlock_m1_wrap();
  pthread_mutex_lock(&m2);
  return 1;
}
