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

void lock_m1() { pthread_mutex_lock(&m1); }
void unlock_m1() { pthread_mutex_unlock(&m1); }

struct Ctx {
  int some_data;
};

struct Ctx ctx;
struct Ctx ctx2;
void lock_m2(struct Ctx* c) { pthread_mutex_lock(&m2); }

// spec: C: 0, H: m1, U: 0
void wrap_lock_m1() {
  // A: 0, H: 0, U: 0
  lock_m1(); // lock(m1)
  // A: {m1}, H: {m1}, U: 0
  // C: {0, {m1}}
  // C \ H: 0, H: 0, U: 0
}

void wrap_unlock_m1() { pthread_mutex_unlock(&m1); }

void wrap_lock_m2(struct Ctx* c) { lock_m2(&ctx); }

void wrap_unlock_m2() { pthread_mutex_unlock(&m2); }
// This should deadlock with direct_m2_m1(), but the analysis needs to
// see lock_m1() acquires the lock and apply that interprocedurally
// spec: C: {0, m1}, {{m1}, m2}}, H: 0, U: 0
int nested_unbalanced_lock() {
  // A: 0, H: 0, U: 0
  wrap_lock_m1(); // lock(m1);
  // A: {m1}, H: {m1}, U: 0
  wrap_lock_m2(&ctx2);
  // A: {m1, m2}, H: {m1, m2}, U: 0
  wrap_unlock_m2();
  // A: {m1, m2}, H: {m1}, U: 0
  wrap_unlock_m1();
  // A: {m1, m2}, H: 0, U: 0
  // C: {{0, m1}, {{m2}, m1}}

  return 0;
}

int direct_m2_m1_lock() {
  pthread_mutex_lock(&m2);
  pthread_mutex_lock(&m1);
  pthread_mutex_unlock(&m1);
  pthread_mutex_unlock(&m2);
  return 1;
}
