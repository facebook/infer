/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdio.h>

#include "thread.h"

#define THREAD_NAME_SIZE 100
#define NUM_PUSH_THREADS 2
#define NUM_POP_THREADS 2
static_assert(
    NUM_PUSH_THREADS >= NUM_POP_THREADS, "#push threads >= #pop threads");

/**
 * An implementation of Treiber's linearizable list-based stack.
 *
 * Treiber, R.K.: Systems programming: Coping with parallelism.
 * Technical Report RJ 5118, IBM Almaden Research Center (1986)
 */

/* error code used to indicate a stack was unexpectedly empty */
#define EMPTY ((error_t)-1)

typedef int data_t;

typedef struct node {
  data_t data;
  _Atomic(struct node*) next;
} node_t;

typedef struct {
  _Atomic(node_t*) top;
} treiber_stack_t;

static treiber_stack_t*
treiber_stack_create(void)
{
  treiber_stack_t* result = __llair_alloc(sizeof(treiber_stack_t));
  atomic_store(&result->top, NULL);
  return result;
}

bool
treiber_stack_is_empty(treiber_stack_t* s)
{
  cct_point();
  node_t* top_snapshot = atomic_load(&s->top);
  return top_snapshot == NULL;
}

void
treiber_stack_push(treiber_stack_t* s, data_t d)
{
  node_t* new_node = __llair_alloc(sizeof(node_t));
  node_t* top_snapshot = NULL;
  bool redirected_top;
  new_node->data = d;
  cct_point();
  top_snapshot = atomic_load(&s->top);
  do {
    atomic_store(&new_node->next, top_snapshot);
    cct_point();
#ifdef PUSH_BUG
    atomic_store(&s->top, new_node);
    redirected_top = true;
#else
    redirected_top =
        atomic_compare_exchange_weak(&s->top, &top_snapshot, new_node);
#endif
  } while (!redirected_top);
}

error_t
treiber_stack_pop(data_t* r, treiber_stack_t* s)
{
  node_t* top_snapshot = NULL;
  node_t* tn = NULL;
  bool redirected_top;
  cct_point();
  top_snapshot = atomic_load(&s->top);
  do {
    if (top_snapshot == NULL) {
      return EMPTY;
    }
    tn = atomic_load(&top_snapshot->next);
    cct_point();
#ifdef POP_BUG
    atomic_store(&s->top, tn);
    redirected_top = true;
#else
    redirected_top = atomic_compare_exchange_weak(&s->top, &top_snapshot, tn);
#endif
  } while (!redirected_top);
  *r = top_snapshot->data;
  return OK;
}

static int
push_thread_run(void* const arg)
{
  if (arg == NULL) {
    return 0;
  }

  treiber_stack_t* s = (treiber_stack_t*)arg;
  data_t val = __llair_choice();
  treiber_stack_push(s, val);
  return val;
}

static int
pop_thread_run(void* const arg)
{
  if (arg == NULL) {
    return 0;
  }

  treiber_stack_t* s = (treiber_stack_t*)arg;
  data_t val;
  error_t status = treiber_stack_pop(&val, s);
  assert(OK == status && "Pop only from non-empty stacks");
  return val;
}

/* First runs all push threads to finish, then runs all pop threads to finish.
 */
int
main(void)
{
  error_t status;

  treiber_stack_t* test_stack = treiber_stack_create();
  char tmp_name[THREAD_NAME_SIZE];
  int thread_ret;
  int32_t total_push = 0;
  int32_t num_pushed = 0;
  int32_t total_pop = 0;
  int32_t num_popped = 0;

  thread_t* push_threads[NUM_PUSH_THREADS];
  for (int i = 0; i < NUM_PUSH_THREADS; i++) {
    snprintf(tmp_name, THREAD_NAME_SIZE * sizeof(char), "push-%d", i);
    status =
        thread_create(&push_threads[i], tmp_name, &push_thread_run, test_stack);
    assert(OK == status && "Thread created successfully");
  }
  for (int i = 0; i < NUM_PUSH_THREADS; i++) {
    thread_resume(push_threads[i]);
  }
  for (int i = 0; i < NUM_PUSH_THREADS; i++) {
    status = thread_join(push_threads[i], &thread_ret, TIME_INFINITE);
    assert(OK == status && "Thread joined successfully");
    total_push += thread_ret;
    if (thread_ret != 0) {
      ++num_pushed;
    }
  }

  thread_t* pop_threads[NUM_POP_THREADS];
  for (int i = 0; i < NUM_POP_THREADS; i++) {
    snprintf(tmp_name, THREAD_NAME_SIZE * sizeof(char), "pop-%d", i);
    status =
        thread_create(&pop_threads[i], tmp_name, &pop_thread_run, test_stack);
    assert(OK == status && "Thread created successfully");
  }
  for (int i = 0; i < NUM_POP_THREADS; i++) {
    thread_resume(pop_threads[i]);
  }
  for (int i = 0; i < NUM_POP_THREADS; i++) {
    status = thread_join(pop_threads[i], &thread_ret, TIME_INFINITE);
    assert(OK == status && "Thread joined successfully");
    total_pop += thread_ret;
    if (thread_ret != 0) {
      ++num_popped;
    }
  }

  assert(num_pushed - num_popped == NUM_PUSH_THREADS - NUM_POP_THREADS &&
      "Number of remaining elements = #push threads - #pop threads");
  assert(total_push >= total_pop &&
      "sum of pushed elements >= sum of popped elements");

  /* check stack has length NUM_PUSH_THREADS - NUM_POP_THREADS */
  for (int i = 0; i < NUM_PUSH_THREADS - NUM_POP_THREADS; i++) {
    data_t val;
    status = treiber_stack_pop(&val, test_stack);
    assert(OK == status && "Pop only from non-empty stacks");
  }
  assert(treiber_stack_is_empty(test_stack) && "Stack is empty");

  return 0;
}
