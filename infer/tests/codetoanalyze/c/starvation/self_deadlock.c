/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// a C struct or union depending on the libc
typedef union {
  char __size[40];
  long __align;
} pthread_mutex_t;
int pthread_mutex_lock(pthread_mutex_t*);
int pthread_mutex_unlock(pthread_mutex_t*);

struct Device {
  pthread_mutex_t mutex;
  int count;
};

void lock_twice_bad(struct Device* d) {
  pthread_mutex_lock(&d->mutex);
  pthread_mutex_lock(&d->mutex);
  d->count++;
  pthread_mutex_unlock(&d->mutex);
  pthread_mutex_unlock(&d->mutex);
}

void increment(struct Device* d) {
  pthread_mutex_lock(&d->mutex);
  d->count++;
  pthread_mutex_unlock(&d->mutex);
}

void call_under_lock_bad(struct Device* d) {
  pthread_mutex_lock(&d->mutex);
  increment(d);
  pthread_mutex_unlock(&d->mutex);
}

void lock_sequentially_ok(struct Device* d) {
  increment(d);
  pthread_mutex_lock(&d->mutex);
  d->count--;
  pthread_mutex_unlock(&d->mutex);
}

void lock_two_devices_ok(struct Device* d1, struct Device* d2) {
  pthread_mutex_lock(&d1->mutex);
  pthread_mutex_lock(&d2->mutex);
  pthread_mutex_unlock(&d2->mutex);
  pthread_mutex_unlock(&d1->mutex);
}

pthread_mutex_t global_mutex;

void lock_global() {
  pthread_mutex_lock(&global_mutex);
  pthread_mutex_unlock(&global_mutex);
}

void lock_global_twice_bad() {
  pthread_mutex_lock(&global_mutex);
  lock_global();
  pthread_mutex_unlock(&global_mutex);
}

// locks of unknown types are assumed to be recursive
struct UnknownMutex {
  int state;
};
void unknown_mutex_lock(struct UnknownMutex*);
void unknown_mutex_unlock(struct UnknownMutex*);

void unknown_mutex_relock_ok(struct UnknownMutex* m) {
  unknown_mutex_lock(m);
  unknown_mutex_lock(m);
  unknown_mutex_unlock(m);
  unknown_mutex_unlock(m);
}

// declared non-recursive in .inferconfig
struct ConfiguredMutex {
  int state;
};
void configured_mutex_lock(struct ConfiguredMutex*);
void configured_mutex_unlock(struct ConfiguredMutex*);

void configured_mutex_relock_bad(struct ConfiguredMutex* m) {
  configured_mutex_lock(m);
  configured_mutex_lock(m);
  configured_mutex_unlock(m);
  configured_mutex_unlock(m);
}

// array indices are not tracked, so the elements of an array of locks are
// assumed to be distinct, eg with lock striping
pthread_mutex_t stripes[16];

void lock_two_stripes_ok(int i, int j) {
  pthread_mutex_lock(&stripes[i]);
  pthread_mutex_lock(&stripes[j]);
  pthread_mutex_unlock(&stripes[j]);
  pthread_mutex_unlock(&stripes[i]);
}

void lock_two_hashed_stripes_ok(unsigned a, unsigned b) {
  pthread_mutex_lock(&stripes[a % 16]);
  pthread_mutex_lock(&stripes[b % 16]);
  pthread_mutex_unlock(&stripes[b % 16]);
  pthread_mutex_unlock(&stripes[a % 16]);
}

void lock_mutex(pthread_mutex_t* m) {
  pthread_mutex_lock(m);
  pthread_mutex_unlock(m);
}

void lock_stripe_in_callee_ok(int i, int j) {
  pthread_mutex_lock(&stripes[i]);
  lock_mutex(&stripes[j]);
  pthread_mutex_unlock(&stripes[i]);
}

void lock_both(pthread_mutex_t* m1, pthread_mutex_t* m2) {
  pthread_mutex_lock(m1);
  pthread_mutex_lock(m2);
  pthread_mutex_unlock(m2);
  pthread_mutex_unlock(m1);
}

void lock_two_stripes_in_callee_ok(int i, int j) {
  lock_both(&stripes[i], &stripes[j]);
}

void lock_device_twice_in_callee_bad(struct Device* d) {
  lock_both(&d->mutex, &d->mutex);
}

void lock_two_elements_ok(pthread_mutex_t* locks, int i, int j) {
  pthread_mutex_lock(&locks[i]);
  pthread_mutex_lock(&locks[j]);
  pthread_mutex_unlock(&locks[j]);
  pthread_mutex_unlock(&locks[i]);
}

struct Table {
  struct Device buckets[16];
};

void lock_two_buckets_ok(struct Table* t, int i, int j) {
  pthread_mutex_lock(&t->buckets[i].mutex);
  pthread_mutex_lock(&t->buckets[j].mutex);
  pthread_mutex_unlock(&t->buckets[j].mutex);
  pthread_mutex_unlock(&t->buckets[i].mutex);
}

pthread_mutex_t* stripe_pointers[16];

void lock_two_stripe_pointers_ok(int i, int j) {
  pthread_mutex_lock(stripe_pointers[i]);
  pthread_mutex_lock(stripe_pointers[j]);
  pthread_mutex_unlock(stripe_pointers[j]);
  pthread_mutex_unlock(stripe_pointers[i]);
}

pthread_mutex_t grid[4][4];

void lock_two_cells_ok(int i, int j) {
  pthread_mutex_lock(&grid[i][j]);
  pthread_mutex_lock(&grid[j][i]);
  pthread_mutex_unlock(&grid[j][i]);
  pthread_mutex_unlock(&grid[i][j]);
}

// the same element is locked twice, but the index is not tracked
void FN_lock_stripe_twice_bad(int i) {
  pthread_mutex_lock(&stripes[i]);
  pthread_mutex_lock(&stripes[i]);
  pthread_mutex_unlock(&stripes[i]);
  pthread_mutex_unlock(&stripes[i]);
}

void stripe_then_global_bad(int i) {
  pthread_mutex_lock(&stripes[i]);
  lock_global();
  pthread_mutex_unlock(&stripes[i]);
}

void global_then_stripe_bad(int i) {
  pthread_mutex_lock(&global_mutex);
  lock_mutex(&stripes[i]);
  pthread_mutex_unlock(&global_mutex);
}

// the objects returned by two calls are assumed to be distinct
struct Device* get_device(int i);

void lock_device(struct Device* d) { pthread_mutex_lock(&d->mutex); }

void unlock_device(struct Device* d) { pthread_mutex_unlock(&d->mutex); }

void lock_two_returned_devices_ok() {
  lock_device(get_device(0));
  lock_device(get_device(1));
  unlock_device(get_device(1));
  unlock_device(get_device(0));
}

// both calls may return the same object, but returned objects are not tracked
void FN_lock_returned_device_twice_bad() {
  lock_device(get_device(0));
  lock_device(get_device(0));
  unlock_device(get_device(0));
  unlock_device(get_device(0));
}
