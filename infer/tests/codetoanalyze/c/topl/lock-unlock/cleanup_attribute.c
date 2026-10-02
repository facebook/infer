/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct FakeMut {
  int blah;
};
int pthread_mutex_lock(struct FakeMut* mut);
void pthread_mutex_unlock(struct FakeMut*);

struct FakeMut cleanup_m;

void unlock_cleanup(struct FakeMut** m) { pthread_mutex_unlock(*m); }

void no_unlock_cleanup(struct FakeMut** m) {}

void cleanup_unlock_ok() {
  if (pthread_mutex_lock(&cleanup_m) == 0) {
    __attribute__((cleanup(unlock_cleanup))) struct FakeMut* guard = &cleanup_m;
  }
}

void cleanup_no_unlock_bad() {
  if (pthread_mutex_lock(&cleanup_m) == 0) {
    __attribute__((cleanup(no_unlock_cleanup))) struct FakeMut* guard =
        &cleanup_m;
  }
}
