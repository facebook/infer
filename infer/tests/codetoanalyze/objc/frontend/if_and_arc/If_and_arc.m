/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <Foundation/NSObject.h>

id foo();

bool foo_bool();

int call_bool_function_in_and_cond_in_if() {
  int* p;
  if (p && foo_bool()) {
    return 5;
  } else
    return 4;
}

int call_object_function_in_and_cond_in_if() {
  int* p;
  if (p && foo()) {
    return 5;
  } else
    return 4;
}
