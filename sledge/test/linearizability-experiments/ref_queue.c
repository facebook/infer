/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "ref_queue.h"
#include <stdio.h>
#include <stdlib.h>

ref_queue* new_ref_queue(int depth) {
  ref_queue* result = malloc(sizeof(ref_queue));
  result->data = malloc(sizeof(data_type) * depth);
  result->depth = depth;
  result->size = 0;
  result->start = 0;
  result->end = 0;
  return result;
}

bool ref_enqueue(ref_queue* q, data_type v) {
  if (q->size == q->depth) {
    return false;
  }
  ++q->size;
  q->data[q->end] = v;
  ++q->end;
  if (q->end == q->depth) {
    q->end = 0;
  }
  // printf("ref_enqueue: %d\n", v);
  return true;
}

data_type ref_dequeue(ref_queue* q) {
  if (q->size == 0) {
    return EMPTY;
  }
  data_type result = q->data[q->start];
  ++q->start;
  if (q->start == q->depth) {
    q->start = 0;
  }
  --q->size;
  // printf("ref_dequeue: %d\n", result);
  return result;
}

void print_ref_queue(ref_queue* q) {}
