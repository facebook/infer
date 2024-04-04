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

struct FakeMut m1;
struct FakeMut m2;

int simple_null_pointer() {
  pthread_mutex_lock1(&m1);
  pthread_mutex_lock(&m2);
  return 0;
}

int null_pointer_interproc() {
  pthread_mutex_lock(&m2);
  pthread_mutex_lock1(&m1);
  return 1;
}
