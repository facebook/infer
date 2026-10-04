/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct AliasMut {
  int blah;
};
void pthread_mutex_lock(struct AliasMut*);
void pthread_mutex_unlock(struct AliasMut*);

struct AliasMut alias_m1;
struct AliasMut alias_m2;

struct AliasMut* get_alias_m1() { return &alias_m1; }

int alias_m1_then_m2_bad() {
  pthread_mutex_lock(get_alias_m1());
  pthread_mutex_lock(&alias_m2);
  pthread_mutex_unlock(&alias_m2);
  pthread_mutex_unlock(get_alias_m1());
  return 0;
}

int alias_m2_then_m1_bad() {
  pthread_mutex_lock(&alias_m2);
  pthread_mutex_lock(get_alias_m1());
  pthread_mutex_unlock(get_alias_m1());
  pthread_mutex_unlock(&alias_m2);
  return 0;
}
