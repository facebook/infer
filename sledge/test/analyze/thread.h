/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/*
 * API for threads built from analyzer intrinsics
 */

#include <stdint.h>

typedef int error_t;
#define OK 0

error_t
thread_create(
    thread_t** t, const char* name, thread_create_routine entry, void* arg)
{
  thread_t* child = __llair_alloc(sizeof(thread_t));
  *child = sledge_thread_create(entry, arg);
  *t = child;
  return OK;
}

bool
thread_resume(thread_t* thread)
{
  sledge_thread_resume(*thread);
  return true;
}

#define TIME_INFINITE UINT64_MAX

error_t
thread_join(thread_t* thread, int* ret_code, uint64_t timeout)
{
  *ret_code = sledge_thread_join(*thread);
  return OK;
}
