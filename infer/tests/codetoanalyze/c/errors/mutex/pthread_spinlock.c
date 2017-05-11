/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include <pthread.h>

#ifdef __APPLE__
// Spinlocks do not exist on mac
typedef struct {
  int __whatever;
} pthread_spinlock_t;
extern int pthread_spin_init(pthread_spinlock_t *__lock, int __pshared);
extern int pthread_spin_destroy (pthread_spinlock_t *__lock);
extern int pthread_spin_lock (pthread_spinlock_t *__lock);
extern int pthread_spin_trylock (pthread_spinlock_t *__lock);
extern int pthread_spin_unlock (pthread_spinlock_t *__lock);
#endif

void spinlock_double_lock_bad(pthread_spinlock_t* m) {
  pthread_spin_lock(m);
  pthread_spin_lock(m);
}

void spinlock_double_lock_bad2() {
  pthread_spinlock_t m;
  spinlock_double_lock_bad(&m);
}
