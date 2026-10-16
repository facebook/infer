/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <err.h>
#include <pthread.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <unistd.h>

void fatal_error_noreturn(const char* msg) __attribute__((__noreturn__));

_Noreturn void die(void);

void log_assert_fail(const char* cond, const char* fmt, ...)
    __attribute__((__noreturn__));

#define LOG_FATAL_IF(cond, ...) \
  ((cond) ? log_assert_fail(#cond, __VA_ARGS__) : (void)0)

// as declared in <threads.h>, which not every C library provides
_Noreturn void thrd_exit(int res);

void malloc_then_noreturn_ok(int x) {
  int* p = (int*)malloc(sizeof(int));
  if (x < 0) {
    fatal_error_noreturn("error");
  }
  free(p);
}

void malloc_then_c11_noreturn_ok(int x) {
  int* p = (int*)malloc(sizeof(int));
  if (x < 0) {
    die();
  }
  free(p);
}

void malloc_then__exit_ok(int x) {
  int* p = (int*)malloc(sizeof(int));
  if (x < 0) {
    _exit(1);
  }
  free(p);
}

void malloc_then_err_ok(int x) {
  int* p = (int*)malloc(sizeof(int));
  if (x < 0) {
    err(1, "error");
  }
  free(p);
}

void fatal_error_wrapper(const char* msg) { fatal_error_noreturn(msg); }

void malloc_then_noreturn_wrapper_ok(int x) {
  int* p = (int*)malloc(sizeof(int));
  if (x < 0) {
    fatal_error_wrapper("error");
  }
  free(p);
}

void socket_then_fatal_if_ok(int x) {
  int fd = socket(AF_UNIX, SOCK_STREAM, 0);
  LOG_FATAL_IF(x < 0, "error %d", x);
  close(fd);
}

void fopen_then_noreturn_ok(int x) {
  FILE* f = fopen("file.txt", "r");
  if (x < 0) {
    fatal_error_noreturn("error");
  }
  if (f) {
    fclose(f);
  }
}

void malloc_leak_on_return_path_bad(int x) {
  int* p = (int*)malloc(sizeof(int));
  if (x < 0) {
    fatal_error_noreturn("error");
  }
}

void fopen_leak_on_return_path_bad(int x) {
  FILE* f = fopen("file.txt", "r");
  LOG_FATAL_IF(x < 0, "error %d", x);
}

void* malloc_then_pthread_exit_bad(void* arg) {
  int* p = (int*)malloc(sizeof(int));
  pthread_exit(NULL);
}

int malloc_then_thrd_exit_bad(void* arg) {
  int* p = (int*)malloc(sizeof(int));
  thrd_exit(0);
}

void* malloc_then_pthread_exit_retval_ok(void* arg) {
  int* p = (int*)malloc(sizeof(int));
  pthread_exit(p);
}

// the leak happens before the call, but leaks are only detected when the path
// ends, where they are ignored as at exit()
void FN_leak_before_noreturn_bad(int x) {
  int* p = (int*)malloc(sizeof(int));
  p = NULL;
  fatal_error_noreturn("error");
}

static jmp_buf env;

void longjmp_to_env(void) __attribute__((__noreturn__));

// the call is treated as the end of the program, but execution resumes at
// setjmp, outside the scope of p
void FN_setjmp_recovery_through_noreturn_bad(void) {
  if (setjmp(env) == 0) {
    int* p = (int*)malloc(sizeof(int));
    longjmp_to_env();
  }
}
