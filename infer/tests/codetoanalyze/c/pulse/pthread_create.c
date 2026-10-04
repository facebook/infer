/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <pthread.h>
#include <stdlib.h>

void dummy() {}

void deref_pointer(int* x) { int y = *x; }

int pthread_create_dummy_ok() {
  pthread_t thread;
  return pthread_create(&thread, NULL, dummy, NULL);
}

int pthread_create_deref_bad() {
  pthread_t thread;
  return pthread_create(&thread, NULL, deref_pointer, NULL);
}

int pthread_create_deref_ok() {
  pthread_t thread;
  int x;
  return pthread_create(&thread, NULL, deref_pointer, &x);
}

extern void some_unknown_function(void);

int pthread_unknown_ok() {
  pthread_t thread;
  return pthread_create(&thread, NULL, some_unknown_function, NULL);
}

// pthread_create returns 0 or an error number
int pthread_create_negative_result_ok() {
  pthread_t thread;
  int* p = NULL;
  if (pthread_create(&thread, NULL, dummy, NULL) < 0) {
    return *p;
  }
  return pthread_join(thread, NULL);
}

void* deref_arg(void* arg) {
  int* p = arg;
  return (void*)(long)*p;
}

int pthread_create_freed_arg_bad() {
  pthread_t thread;
  int* p = malloc(sizeof(int));
  if (p == NULL) {
    return 1;
  }
  free(p);
  return pthread_create(&thread, NULL, deref_arg, p);
}

int pthread_create_cast_routine_bad() {
  pthread_t thread;
  return pthread_create(&thread, NULL, (void* (*)(void*))deref_pointer, NULL);
}

struct thread_data {
  int* value;
  int flag;
};

void* deref_value(void* arg) {
  struct thread_data* data = arg;
  return (void*)(long)*data->value;
}

int pthread_create_null_field_bad() {
  pthread_t thread;
  struct thread_data data = {NULL, 0};
  int ret = pthread_create(&thread, NULL, deref_value, &data);
  pthread_join(thread, NULL);
  return ret;
}

int pthread_create_field_ok() {
  pthread_t thread;
  int x = 0;
  struct thread_data data = {&x, 0};
  int ret = pthread_create(&thread, NULL, deref_value, &data);
  pthread_join(thread, NULL);
  return ret;
}

struct handoff {
  pthread_mutex_t lock;
  int* value;
};

void* deref_value_locked(void* arg) {
  struct handoff* handoff = arg;
  pthread_mutex_lock(&handoff->lock);
  int value = *handoff->value;
  pthread_mutex_unlock(&handoff->lock);
  return (void*)(long)value;
}

// FP: the start routine is checked against the state where the thread is
// created, the lock that makes the thread wait until [value] is set is ignored
int FP_pthread_create_set_value_after_create_ok() {
  pthread_t thread;
  int x = 0;
  struct handoff handoff = {.value = NULL};
  pthread_mutex_init(&handoff.lock, NULL);
  pthread_mutex_lock(&handoff.lock);
  if (pthread_create(&thread, NULL, deref_value_locked, &handoff) != 0) {
    pthread_mutex_unlock(&handoff.lock);
    return 1;
  }
  handoff.value = &x;
  pthread_mutex_unlock(&handoff.lock);
  return pthread_join(thread, NULL);
}

void* free_arg(void* arg) {
  free(arg);
  return NULL;
}

// the thread owns [p] only if it is created
int pthread_create_ownership_transfer_ok() {
  pthread_t thread;
  int* p = malloc(sizeof(int));
  if (p == NULL) {
    return 1;
  }
  if (pthread_create(&thread, NULL, free_arg, p) != 0) {
    free(p);
    return 1;
  }
  return pthread_detach(thread);
}

static int* shared;
static int stop;

void* free_shared_when_stopped(void* arg) {
  while (!stop) {
    shared[0]++;
  }
  free(shared);
  shared = NULL;
  return NULL;
}

// the thread frees [shared] only once the caller is done with it
int pthread_create_thread_frees_global_later_ok() {
  pthread_t thread;
  shared = malloc(2 * sizeof(int));
  if (shared == NULL) {
    return 1;
  }
  if (pthread_create(&thread, NULL, free_shared_when_stopped, NULL) != 0) {
    return 1;
  }
  shared[1] = 1;
  stop = 1;
  return pthread_join(thread, NULL);
}

void* exit_thread(void* arg) { pthread_exit(NULL); }

// the caller keeps running when the thread exits
int pthread_create_exiting_routine_bad() {
  pthread_t thread;
  int* p = NULL;
  pthread_create(&thread, NULL, exit_thread, NULL);
  return *p;
}

void* null_deref_thread_bad(void* arg) {
  int* p = NULL;
  *p = 0;
  return NULL;
}

// the issue of [null_deref_thread_bad] does not stop the paths of the caller,
// they would take up the disjuncts needed for the path to the issue below
int create_thread_on_paths_latent(int a, int b, int c, int d) {
  int n = 0;
  if (a) {
    n++;
  }
  if (b) {
    n++;
  }
  if (c) {
    n++;
  }
  if (d) {
    n++;
  }
  pthread_t thread;
  pthread_create(&thread, NULL, null_deref_thread_bad, NULL);
  int* p = NULL;
  if (n == 0) {
    return *p;
  }
  return n;
}

int create_thread_on_paths_bad() {
  return create_thread_on_paths_latent(0, 0, 0, 0);
}

void* deref_value_if_flag(void* arg) {
  struct thread_data* data = arg;
  if (data->flag == 4) {
    return (void*)(long)*data->value;
  }
  return NULL;
}

int start_thread_with_flag_latent(int flag) {
  pthread_t thread;
  struct thread_data data = {NULL, flag};
  int ret = pthread_create(&thread, NULL, deref_value_if_flag, &data);
  pthread_join(thread, NULL);
  return ret;
}

int pthread_create_latent_manifest_bad() {
  return start_thread_with_flag_latent(4);
}

// reported when [start_routine_on_null_bad] is specialized for the routine
// passed by [start_deref_arg_on_null]
int start_routine_on_null_bad(void* (*routine)(void*)) {
  pthread_t thread;
  return pthread_create(&thread, NULL, routine, NULL);
}

int start_deref_arg_on_null() { return start_routine_on_null_bad(deref_arg); }

int start_deref_thread(int* p) {
  pthread_t thread;
  return pthread_create(&thread, NULL, deref_arg, p);
}

// FN: only the issues of the start routine are reported, what it requires from
// [p] is not propagated to the callers of [start_deref_thread]
int FN_pthread_create_through_wrapper_bad() { return start_deref_thread(NULL); }

// starting a thread is not a recursive call
void* respawn_while_flag_ok(void* arg) {
  struct thread_data* data = arg;
  if (data->flag) {
    pthread_t thread;
    pthread_create(&thread, NULL, respawn_while_flag_ok, data);
    pthread_detach(thread);
  }
  return NULL;
}
