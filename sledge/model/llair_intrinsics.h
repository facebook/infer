/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/* allocation that cannot fail */
void* __llair_alloc(unsigned size);

/* non-deterministic choice */
int __llair_choice();

/* throw an exception */
__attribute__((noreturn)) void __llair_throw(void* thrown_exception);

/* executions that call __llair_unreachable are assumed to be impossible */
__attribute__((noreturn)) void __llair_unreachable();

/* glibc version */
#define __assert_fail(assertion, file, line, function) abort()

/* macos version */
#define __assert_rtn(function, file, line, assertion) abort()

/*
 * threads
 */

typedef int thread_t;

typedef void (*thread_create_routine)();

thread_t sledge_thread_create(thread_create_routine entry);

void sledge_thread_join(thread_t thread);

typedef int error_t;
#define OK 0

error_t thread_create(thread_t** t, thread_create_routine entry) {
  thread_t* child = __llair_alloc(sizeof(thread_t));
  *child = sledge_thread_create(entry);
  *t = child;
  return OK;
}

error_t thread_join(thread_t* t) {
  sledge_thread_join(*t);
  return OK;
}

#ifdef __cplusplus
}
#endif
