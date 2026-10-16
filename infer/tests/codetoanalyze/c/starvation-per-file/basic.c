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

struct FakeMut m1;
struct FakeMut m2;
int x;

// deadlock between globals_1_then_2_bad() and globals_2_then_1_bad()
void globals_1_then_2_bad() {
  pthread_mutex_lock(&m1);
  pthread_mutex_lock(&m2);
  x++;
  pthread_mutex_unlock(&m2);
  pthread_mutex_unlock(&m1);
}

void globals_2_then_1_bad() {
  pthread_mutex_lock(&m2);
  pthread_mutex_lock(&m1);
  x--;
  pthread_mutex_unlock(&m1);
  pthread_mutex_unlock(&m2);
}

static struct FakeMut static_m1;
static struct FakeMut static_m2;

void lock_static_m1() {
  pthread_mutex_lock(&static_m1);
  x++;
  pthread_mutex_unlock(&static_m1);
}

// deadlock between statics_1_then_2_bad() and statics_2_then_1_bad()
void statics_1_then_2_bad() {
  pthread_mutex_lock(&static_m1);
  pthread_mutex_lock(&static_m2);
  x++;
  pthread_mutex_unlock(&static_m2);
  pthread_mutex_unlock(&static_m1);
}

void statics_2_then_1_bad() {
  pthread_mutex_lock(&static_m2);
  lock_static_m1();
  pthread_mutex_unlock(&static_m2);
}

struct Device {
  struct FakeMut a;
  struct FakeMut b;
  int x;
};

// deadlock between device_a_then_b_bad() and device_b_then_a_bad()
void device_a_then_b_bad(struct Device* d) {
  pthread_mutex_lock(&d->a);
  pthread_mutex_lock(&d->b);
  d->x++;
  pthread_mutex_unlock(&d->b);
  pthread_mutex_unlock(&d->a);
}

void device_b_then_a_bad(struct Device* d) {
  pthread_mutex_lock(&d->b);
  pthread_mutex_lock(&d->a);
  d->x--;
  pthread_mutex_unlock(&d->a);
  pthread_mutex_unlock(&d->b);
}

struct Device global_device;

// deadlock between global_device_a_then_b_bad() and
// global_device_b_then_a_bad()
void global_device_a_then_b_bad() {
  pthread_mutex_lock(&global_device.a);
  pthread_mutex_lock(&global_device.b);
  global_device.x++;
  pthread_mutex_unlock(&global_device.b);
  pthread_mutex_unlock(&global_device.a);
}

void global_device_b_then_a_bad() {
  pthread_mutex_lock(&global_device.b);
  pthread_mutex_lock(&global_device.a);
  global_device.x--;
  pthread_mutex_unlock(&global_device.a);
  pthread_mutex_unlock(&global_device.b);
}

struct FakeMut ordered_m1;
struct FakeMut ordered_m2;

// same order everywhere, no deadlock
void ordered_1_ok() {
  pthread_mutex_lock(&ordered_m1);
  pthread_mutex_lock(&ordered_m2);
  x++;
  pthread_mutex_unlock(&ordered_m2);
  pthread_mutex_unlock(&ordered_m1);
}

void ordered_2_ok() {
  pthread_mutex_lock(&ordered_m1);
  pthread_mutex_lock(&ordered_m2);
  x--;
  pthread_mutex_unlock(&ordered_m2);
  pthread_mutex_unlock(&ordered_m1);
}

void increment_under(struct FakeMut* m) {
  pthread_mutex_lock(m);
  x++;
  pthread_mutex_unlock(m);
}

struct FakeMut helper_m1;
struct FakeMut helper_m2;

// deadlock between helper_1_then_2_bad() and helper_2_then_1_bad(), where the
// helper takes the global passed to it
void helper_1_then_2_bad() {
  pthread_mutex_lock(&helper_m1);
  increment_under(&helper_m2);
  pthread_mutex_unlock(&helper_m1);
}

void helper_2_then_1_bad() {
  pthread_mutex_lock(&helper_m2);
  increment_under(&helper_m1);
  pthread_mutex_unlock(&helper_m2);
}

void increment_under_device_b(struct Device* d) {
  pthread_mutex_lock(&d->b);
  d->x++;
  pthread_mutex_unlock(&d->b);
}

// FN: deadlock with global_device_b_then_a_bad(), but the lock taken by the
// helper is rooted at global_device with the type of &global_device, so it is
// not the same lock as &global_device.b
void FN_global_device_a_then_helper_b_bad() {
  pthread_mutex_lock(&global_device.a);
  increment_under_device_b(&global_device);
  pthread_mutex_unlock(&global_device.a);
}

struct WrappedMut {
  struct FakeMut m;
};

void wrapped_lock(struct WrappedMut* w) { pthread_mutex_lock(&w->m); }
void wrapped_unlock(struct WrappedMut* w) { pthread_mutex_unlock(&w->m); }

struct Conn {
  struct WrappedMut mtx;
};

struct Pipe {
  struct WrappedMut mtx;
};

struct WrappedMut wrapped_global;

// the locks that the wrappers take through local pointers cannot be expressed
// in the callers, so they are ignored instead of being confused with each other
void global_then_local_conn_ok(void* arg) {
  struct Conn* c = arg;
  wrapped_lock(&wrapped_global);
  wrapped_lock(&c->mtx);
  wrapped_unlock(&c->mtx);
  wrapped_unlock(&wrapped_global);
}

void local_pipe_then_global_ok(void* arg) {
  struct Pipe* p = arg;
  wrapped_lock(&p->mtx);
  wrapped_lock(&wrapped_global);
  wrapped_unlock(&wrapped_global);
  wrapped_unlock(&p->mtx);
}

// the locks of distinct structs that end with the same field are not confused
void global_then_conn_ok(struct Conn* c) {
  wrapped_lock(&wrapped_global);
  wrapped_lock(&c->mtx);
  wrapped_unlock(&c->mtx);
  wrapped_unlock(&wrapped_global);
}

void pipe_then_global_ok(struct Pipe* p) {
  wrapped_lock(&p->mtx);
  wrapped_lock(&wrapped_global);
  wrapped_unlock(&wrapped_global);
  wrapped_unlock(&p->mtx);
}
