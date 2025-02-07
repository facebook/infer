/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifndef REF_STACK
#define REF_STACK

typedef int data_type;

typedef struct {
  data_type* data;
  int size;
  int max_size;
} ref_stack;

ref_stack* new_ref_stack(int max_size);
void ref_stack_push(ref_stack* s, data_type v);
data_type ref_stack_pop(ref_stack* s);

#endif
