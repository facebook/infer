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
void pthread_mutex_unlock(struct FakeMut*);

struct FakeMut dedup_m1;
struct FakeMut dedup_m2;

/*
 * This might deadlock or might not, depending on functions
 * direct_one_way_bad and indirect_one_way_bad always being run on the
 * same thread or not.
 *
 * --starvation-whole-program assumes these can run in parallel
 */

int direct_one_way_bad() {
  pthread_mutex_lock(&dedup_m1);
  pthread_mutex_lock(&dedup_m2);
  pthread_mutex_unlock(&dedup_m2);
  pthread_mutex_unlock(&dedup_m1);
  return 0;
}

void indirect_one_way_bad() {
  direct_one_way_bad();
}

int main() {
  pthread_mutex_lock(&dedup_m2);
  indirect_one_way_bad();
  pthread_mutex_unlock(&dedup_m2);
  return 0;
}
