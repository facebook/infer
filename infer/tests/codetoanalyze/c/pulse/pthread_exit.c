/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <pthread.h>
#include <stdlib.h>
#include <sys/socket.h>

void* pthread_exit_stack_address_bad(void* arg) {
  int status = 0;
  pthread_exit(&status);
}

static int exit_status;

void* pthread_exit_global_address_ok(void* arg) { pthread_exit(&exit_status); }

void* pthread_exit_arg_ok(void* arg) { pthread_exit(arg); }

void* socket_then_pthread_exit_bad(void* arg) {
  int fd = socket(AF_UNIX, SOCK_STREAM, 0);
  pthread_exit(NULL);
}

void exit_thread_helper(void) { pthread_exit(NULL); }

// a call to a function that always ends the thread is treated like a call to
// exit(), where leaks are not reported
void* FN_malloc_then_wrapped_pthread_exit_bad(void* arg) {
  int* p = (int*)malloc(sizeof(int));
  exit_thread_helper();
  return NULL;
}

// pthread_exit runs the cleanup handler, which frees p
void* FP_pthread_exit_runs_cleanup_handler_ok(void* arg) {
  int* p = (int*)malloc(sizeof(int));
  pthread_cleanup_push(free, p);
  pthread_exit(NULL);
  pthread_cleanup_pop(0);
}
