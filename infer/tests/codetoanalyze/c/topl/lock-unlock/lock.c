/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct FakeMut {
  int blah;
};
struct FakeMut m1;
int pthread_mutex_lock(struct FakeMut* mut);
void pthread_mutex_unlock(struct FakeMut*);

void unlock_wrap() { pthread_mutex_unlock(&m1); }
void lock_unlock() {
  pthread_mutex_lock(&m1);
  unlock_wrap();
}
