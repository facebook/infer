/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifndef REF_QUEUE
#define REF_QUEUE

#include <stdbool.h>

typedef int data_type;
#define EMPTY -1

typedef struct {
  data_type* data;
  int depth;
  int size;
  int start;
  int end;
} ref_queue;

ref_queue* new_ref_queue(int depth);
bool ref_enqueue(ref_queue* q, data_type v);
data_type ref_dequeue(ref_queue* q);
void print_ref_queue(ref_queue* q);

#endif
