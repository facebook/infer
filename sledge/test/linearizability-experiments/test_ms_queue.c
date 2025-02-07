/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Testing the non-blocking queue described in
// "Simple, Fast, and Practical Non-Blocking and Blocking
// Concurrent Queue Algorithms"
// (https://www.cs.rochester.edu/~scott/papers/1996_PODC_queues.pdf)

#include <assert.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "ref_queue.h"
#include "scheduler.h"

#define NUM_THREADS 2
#define CONTEXT_SWITCHES_BOUND 1
#define SCHEDULE_POINTS 13
#include "test_mains.h"

typedef int data_type;
#define EMPTY -1

typedef struct node_t {
  data_type d;
  struct node_t* n;
} node;

typedef struct queue_t {
  node* head;
  node* tail;
} queue;

typedef enum { ENQUEUE, DEQUEUE } frame_type;

typedef struct {
  queue* q;
  ref_queue* ref;
  data_type v;
  node* new_node;
  node* tail;
  node* next;
} enqueue_frame;

typedef struct {
  queue* q;
  ref_queue* ref;
  data_type result;
  node* head;
  node* tail;
  node* next;
} dequeue_frame;

void init_queue(queue* q) {
  node* dummy = malloc(sizeof(node));
  dummy->n = NULL;
  q->head = dummy;
  q->tail = dummy;
}

dequeue_frame* make_dequeue_frame(queue* q, ref_queue* ref) {
  dequeue_frame* frame = malloc(sizeof(dequeue_frame));
  frame->q = q;
  frame->ref = ref;
  return frame;
}

enqueue_frame* make_enqueue_frame(queue* q, ref_queue* ref, data_type v) {
  enqueue_frame* frame = malloc(sizeof(enqueue_frame));
  frame->q = q;
  frame->ref = ref;
  frame->v = v;
  return frame;
}

int step_enqueue(enqueue_frame* frame, int pc) {
  queue* q = frame->q;
  bool cas_result;
  switch (pc) {
  case 0:
  case 1:
    frame->new_node = malloc(sizeof(node));
    frame->new_node->d = frame->v;
    frame->new_node->n = NULL;
    pc = 4;
    break;

  case 4:
  case 5:
    frame->tail = q->tail;
    pc = 6;
    break;

  case 6:
    frame->next = frame->tail->n;
    pc = 7;
    break;

  case 7:
    if (frame->tail == q->tail) {
      pc = 8;
    } else {
      pc = 4;
    }
    break;

  case 8:
    if (frame->next == NULL) {
      pc = 9;
    } else {
      pc = 13;
    }
    break;

  case 9:
    if (frame->tail->n == frame->next) { // Fake CAS
      frame->tail->n = frame->new_node;
      cas_result = true;
    } else {
      cas_result = false;
    }
    if (cas_result) {
      bool ref_result = ref_enqueue(frame->ref, frame->v);
      assert(ref_result);
      pc = 17;
    } else {
      pc = 4;
    }
    break;

  case 13:
    if (q->tail == frame->tail) { // Fake CAS
      q->tail = frame->next;
    }
    pc = 4;
    break;

  case 17:
    if (q->tail == frame->tail) { // Fake CAS
      q->tail = frame->new_node;
    }
    pc = PC_DONE;
    break;

  default:
    assert(false);
  }
  return pc;
}

int step_dequeue(dequeue_frame* frame, int pc) {
  queue* q = frame->q;
  bool cas_result;
  switch (pc) {
  case 0:
  case 1:
  case 2:
    frame->head = q->head;
    pc = 3;
    break;

  case 3:
    frame->tail = q->tail;
    pc = 4;
    break;

  case 4:
    frame->next = frame->head->n;
    pc = 5;
    break;

  case 5:
    if (frame->head == q->head) {
      pc = 6;
    } else {
      pc = 2;
    }
    break;

  case 6:
    if (frame->head == frame->tail) {
      pc = 7;
    } else {
      pc = 13;
    }
    break;

  case 7:
    if (frame->next == NULL) {
      bool dequeue_result = false;
      data_type ref_result = ref_dequeue(frame->ref);
      if (ref_result != EMPTY) {
        printf("dequeue_result: %d, ref_result: %d\n", dequeue_result,
               ref_result);
      }
      assert(ref_result == EMPTY);
      pc = PC_DONE;
    } else {
      pc = 10;
    }
    break;

  case 10:
    if (q->tail == frame->tail) { // Fake CAS.
      q->tail = frame->next;
    }
    pc = 2;
    break;

  case 13:
    if (q->head == frame->head) {
      q->head = frame->next;
      cas_result = true;
    } else {
      cas_result = false;
    }
    if (cas_result) {
      data_type result = frame->next->d;
      data_type ref_result = ref_dequeue(frame->ref);
      assert(result == ref_result);
      pc = PC_DONE;
    } else {
      pc = 2;
    }
    break;

  default:
    assert(false);
  }
  return pc;
}

// Runs the operation associated with the given frame until reaching the next
// block. That is, the next statement accessing shared memory.
int step(base_frame* frame) {
  assert(frame != NULL);
  if (frame->derived_frame_type == DEQUEUE) {
    dequeue_frame* derived_frame = (dequeue_frame*)frame->derived_frame;
    assert(derived_frame != NULL);
    return step_dequeue(derived_frame, frame->pc);
  } else if (frame->derived_frame_type == ENQUEUE) {
    enqueue_frame* derived_frame = (enqueue_frame*)frame->derived_frame;
    assert(derived_frame != NULL);
    return step_enqueue(derived_frame, frame->pc);
  } else {
    assert(false);
  }
}

int test_with_schedule(uint8_t* schedule, size_t len) {
  queue test_queue;
  init_queue(&test_queue);
  ref_queue* ref = new_ref_queue(len);
  base_frame frames[NUM_THREADS];
  for (int frame_idx = 0; frame_idx < NUM_THREADS; ++frame_idx) {
    int derived_frame_type = (frame_idx % 2 == 0) ? ENQUEUE : DEQUEUE;
    void* derived_frame = NULL;
    if (derived_frame_type == DEQUEUE) {
      // printf("idx: %d is dequeue\n", frame_idx);
      derived_frame = make_dequeue_frame(&test_queue, ref);
    } else {
      // printf("idx: %d is enqueue\n", frame_idx);
      int data_to_push = frame_idx + 1;
      derived_frame = make_enqueue_frame(&test_queue, ref, data_to_push);
    }
    init_frame(&frames[frame_idx], derived_frame, derived_frame_type);
  }
  execute_schedule(schedule, len, frames, NUM_THREADS, CONTEXT_SWITCHES_BOUND);
  return 0;
}
