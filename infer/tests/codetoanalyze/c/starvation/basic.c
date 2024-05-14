/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct BasicMut {
  int blah;
};
void pthread_mutex_lock(struct BasicMut*);
void pthread_mutex_lock1(struct BasicMut*);
void pthread_mutex_unlock(struct BasicMut*);

struct BasicMut m1;
struct BasicMut m2;

int simple_null_pointer() {
  pthread_mutex_lock1(&m1);
  pthread_mutex_lock(&m2);
  pthread_mutex_unlock(&m2);
  pthread_mutex_unlock(&m1);
  return 0;
}

int null_pointer_interproc() {
  pthread_mutex_lock(&m2);
  pthread_mutex_lock1(&m1);
  pthread_mutex_unlock(&m1);
  pthread_mutex_unlock(&m2);
  return 1;
}
