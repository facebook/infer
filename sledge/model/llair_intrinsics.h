/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdbool.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/* allocation that cannot fail */
void* __llair_alloc(long size);

/* non-deterministic choice */
long __llair_choice();

/* executions that call __llair_unreachable are assumed to be impossible */
__attribute__((noreturn)) void __llair_unreachable();

/* assume a condition */
#define __llair_assume(condition) \
  if (!(condition))               \
  __llair_unreachable()

/* throw an exception */
__attribute__((noreturn)) void __llair_throw(void* thrown_exception);

/* glibc version */
#define __assert_fail(assertion, file, line, function) abort()

/* macos version */
#define __assert_rtn(function, file, line, assertion) abort()

/*
 * threads
 */

typedef int thread_t;

typedef int (*thread_create_routine)(void*);

thread_t sledge_thread_create(thread_create_routine entry, void* arg);

void sledge_thread_resume(thread_t thread);

int sledge_thread_join(thread_t thread);

/*
 * cct
 */

void cct_point(void);

#ifdef __cplusplus
}
#endif
