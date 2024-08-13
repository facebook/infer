/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void external_func(int* const*);

int const_local_no_abduce(int* p) {
  external_func(&p);
  return p ? *p : 0;
  // We shouldn't get a stack address escape warning here
}

void set_ptr(int* ptr, int val) { *ptr = val; }

int set_ptr_local_array() {
  int buf[2];
  set_ptr(buf, 1);
  return buf[0];
}

void FP_set_ptr_local_array_return_true_ok() {
  int* p = NULL;
  if (!set_ptr_local_array())
    // not reachable if the analysis of set_ptr_local_array is correct
    *p = 42;
}

int set_ptr_param_array(int buf[]) {
  set_ptr(buf, 1);
  return buf[0];
}

void FP_set_ptr_param_array_return_true_ok() {
  int buf[2];
  int* p = NULL;
  if (!set_ptr_param_array(buf))
    // not reachable if the analysis of set_ptr_local_array is correct
    *p = 42;
}

void FP_set_ptr_param_array_get_ptr_ok() {
  int x = 0;
  int* p = NULL;
  if (!set_ptr_param_array(&x))
    // not reachable if the analysis of set_ptr_local_array is correct
    *p = 42;
}

void set_ptr_param_array_get_null_bad() {
  // A null pointer dereference is expected here
  set_ptr_param_array(NULL);
}
