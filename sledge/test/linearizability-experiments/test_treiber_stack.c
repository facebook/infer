/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "ref_stack.h"
#include "scheduler.h"
#include "treiber_stack.h"

#define NUM_THREADS 3
#define CONTEXT_SWITCHES_BOUND 1
#define SCHEDULE_POINTS NUM_THREADS * 3
#include "test_mains.h"

#define REF_STACK_DEPTH SCHEDULE_POINTS
#define PUSH_NON_ATOMIC_COMPARE_BUG false
#define POP_NON_ATOMIC_COMPARE_BUG true

typedef enum { PUSH, POP } frame_type;

typedef struct {
  stack* s;
  ref_stack* ref;
  data_type v;
  node* x;
  node* t;
} push_frame;

typedef struct {
  stack* s;
  ref_stack* ref;
  node* t;
  node* tn;
  data_type r;
} pop_frame;

push_frame* make_push_frame(stack* s, ref_stack* ref, data_type v) {
  push_frame* frame = malloc(sizeof(push_frame));
  frame->s = s;
  frame->ref = ref;
  frame->v = v;
  return frame;
}

pop_frame* make_pop_frame(stack* s, ref_stack* ref) {
  pop_frame* frame = malloc(sizeof(pop_frame));
  frame->s = s;
  frame->ref = ref;
  return frame;
}

int step_push(push_frame* frame, int pc) {
  stack* s = frame->s;
  switch (pc) {
  case 0:
    /* We filter this out, since this is the reference's precondition. */
    if (frame->ref->size == frame->ref->max_size) {
      return PC_ABORT;
    }
    frame->x = malloc(sizeof(node));
    frame->x->d = frame->v;
    frame->t = NULL;
    pc = 1;
    break;

  case 1:
    frame->t = atomic_load(&s->top);
    frame->x->n = frame->t;
    pc = 2;
    break;

  case 2:
    if (PUSH_NON_ATOMIC_COMPARE_BUG) {
      s->top = frame->x;
      pc = PC_DONE;
      ref_stack_push(frame->ref, frame->v);
    } else { // Correct implementation.
      if (!atomic_compare_exchange_weak(&s->top, &frame->t, frame->x)) {
        pc = 1;
      } else {
        pc = PC_DONE;
        ref_stack_push(frame->ref, frame->v);
      }
    }
    int size = unsafe_size(s);
    int ref_size = frame->ref->size;
    // assert(size == ref_size);
    break;
  default:
    assert(false);
  }
  return pc;
}

int step_pop(pop_frame* frame, int pc) {
  stack* s = frame->s;
  switch (pc) {
  case 0:
    /* We filter this out, since this is the reference's precondition. */
    if (frame->ref->size == 0) {
      return PC_ABORT;
    }
    frame->t = s->top;
    if (frame->t == NULL) {
      data_type ref_result = ref_stack_pop(frame->ref);
      assert(ref_result == EMPTY);
      pc = PC_DONE;
    } else {
      pc = 1;
    }
    break;

  case 1:
    frame->tn = frame->t->n;
    pc = 2;
    break;

  case 2:
    if (POP_NON_ATOMIC_COMPARE_BUG) {
      s->top = frame->tn;
      data_type r = frame->t->d;
      data_type ref_result = ref_stack_pop(frame->ref);
      assert(r == ref_result);
      pc = PC_DONE;
    } else { // Correct implementation
      if (!atomic_compare_exchange_weak(&s->top, &frame->t, frame->tn)) {
        pc = 0;
      } else {
        data_type r = frame->t->d;
        data_type ref_result = ref_stack_pop(frame->ref);
        assert(r == ref_result);
        // free(frame->t); // Should we test for ABA?
        pc = PC_DONE;
      }
    }
    int size = unsafe_size(s);
    int ref_size = frame->ref->size;
    // assert(size == ref_size);
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
  if (frame->derived_frame_type == POP) {
    pop_frame* derived_frame = (pop_frame*)frame->derived_frame;
    assert(derived_frame != NULL);
    return step_pop(derived_frame, frame->pc);
  } else {
    push_frame* derived_frame = (push_frame*)frame->derived_frame;
    if (derived_frame == NULL) {
    }
    assert(derived_frame != NULL);
    return step_push(derived_frame, frame->pc);
  }
}

void print_test() {
  stack test_stack;
  init_stack(&test_stack);
  print(&test_stack);
  printf("\n");
  push(&test_stack, 1);
  print(&test_stack);
  printf("\n");
  push(&test_stack, 2);
  print(&test_stack);
  printf("\n");
  push(&test_stack, 3);
  print(&test_stack);
  printf("\n");
  assert(pop(&test_stack) == 3);
  print(&test_stack);
  printf("\n");
  assert(pop(&test_stack) == 2);
  print(&test_stack);
  printf("\n");
  assert(pop(&test_stack) == 1);
  print(&test_stack);
  printf("\n");
}

int test_with_schedule(uint8_t* schedule, size_t len) {
  stack test_stack;
  init_stack(&test_stack);
  ref_stack* ref = new_ref_stack(len);
  base_frame frames[NUM_THREADS];
  for (int frame_idx = 0; frame_idx < NUM_THREADS; ++frame_idx) {
    int derived_frame_type = (frame_idx % 2 == 0) ? PUSH : POP;
    void* derived_frame = NULL;
    if (derived_frame_type == POP) {
      // printf("adding pop frame\n");
      derived_frame = make_pop_frame(&test_stack, ref);
    } else {
      // printf("adding push frame\n");
      int data_to_push = frame_idx;
      derived_frame = make_push_frame(&test_stack, ref, data_to_push);
    }
    init_frame(&frames[frame_idx], derived_frame, derived_frame_type);
  }
  execute_schedule(schedule, len, frames, NUM_THREADS, CONTEXT_SWITCHES_BOUND);
  return 0;
}
