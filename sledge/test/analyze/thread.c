/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "thread.h"

int count = 0;

int
child_routine(void* arg)
{
  return count++;
}

int
main()
{
  thread_t* child;
  error_t err = thread_create(&child, "child", &child_routine, NULL);
  thread_resume(child);
  int ret_code;
  err = thread_join(child, &ret_code);
  count += ret_code;
  return count;
}
