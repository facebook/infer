/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct WMutex {
  int blah;
};
void pthread_mutex_lock(struct WMutex*);
void pthread_mutex_unlock(struct WMutex*);

struct WMutex wm1;
struct WMutex wm2;
struct WMutex wm3;

void lock_wrapper(struct WMutex* m) { pthread_mutex_lock(m); }

void unlock_wrapper(struct WMutex* m) { pthread_mutex_unlock(m); }

// the caller has fewer parameters than the wrappers
int wrapped_wm1_wm2_bad() {
  pthread_mutex_lock(&wm1);
  lock_wrapper(&wm2);
  unlock_wrapper(&wm2);
  pthread_mutex_unlock(&wm1);
  return 0;
}

int direct_wm2_wm1_bad() {
  pthread_mutex_lock(&wm2);
  pthread_mutex_lock(&wm1);
  pthread_mutex_unlock(&wm1);
  pthread_mutex_unlock(&wm2);
  return 1;
}

struct WConn {
  struct WMutex* m;
};

void conn_acquire(struct WConn* c, struct WMutex* m) {
  c->m = m;
  pthread_mutex_lock(c->m);
}

void conn_release(struct WConn* c) { pthread_mutex_unlock(c->m); }

// the lock taken and released through the field of the connection is balanced
int conn_wm2_wm1_bad(struct WConn* c) {
  conn_acquire(c, &wm3);
  conn_release(c);
  pthread_mutex_lock(&wm2);
  pthread_mutex_lock(&wm1);
  pthread_mutex_unlock(&wm1);
  pthread_mutex_unlock(&wm2);
  return 0;
}

struct WGuard {
  struct WMutex* m;
};

void guard_init(struct WGuard* g, struct WMutex* m) {
  g->m = m;
  pthread_mutex_lock(m);
}

void guard_release(struct WGuard* g) { pthread_mutex_unlock(g->m); }

// the release through the field of the local guard is not matched with the
// lock taken through the parameter, so wm1 is considered held when wm2 is taken
int FP_guard_wm1_wm2_ok(int b) {
  if (b) {
    struct WGuard g;
    guard_init(&g, &wm1);
    guard_release(&g);
    pthread_mutex_lock(&wm2);
    pthread_mutex_unlock(&wm2);
  }
  return 0;
}

struct WMutex wm4;
struct WMutex wm5;
struct WMutex wm6;
struct WMutex wm7;

struct WToken {
  int id;
};

struct WToken lock_wm4(void) {
  struct WToken t = {4};
  pthread_mutex_lock(&wm4);
  return t;
}

void wrap_lock_wm4(void) { struct WToken t = lock_wm4(); }

// the lock taken by a function returning a struct stays held in its callers
int token_wm4_wm5_bad() {
  wrap_lock_wm4();
  pthread_mutex_lock(&wm5);
  pthread_mutex_unlock(&wm5);
  pthread_mutex_unlock(&wm4);
  return 0;
}

struct WToken lock_wm6_wm7(void) {
  struct WToken t = {6};
  pthread_mutex_lock(&wm6);
  pthread_mutex_lock(&wm7);
  return t;
}

int two_lock_token_wm6_wm5_bad() {
  struct WToken t = lock_wm6_wm7();
  pthread_mutex_lock(&wm5);
  pthread_mutex_unlock(&wm5);
  pthread_mutex_unlock(&wm7);
  pthread_mutex_unlock(&wm6);
  return 0;
}

int direct_wm5_wm4_bad() {
  pthread_mutex_lock(&wm5);
  pthread_mutex_lock(&wm4);
  pthread_mutex_unlock(&wm4);
  pthread_mutex_unlock(&wm5);
  return 0;
}

int direct_wm5_wm6_bad() {
  pthread_mutex_lock(&wm5);
  pthread_mutex_lock(&wm6);
  pthread_mutex_unlock(&wm6);
  pthread_mutex_unlock(&wm5);
  return 0;
}
