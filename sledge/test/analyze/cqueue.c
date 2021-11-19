/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>

uint32_t
cct_random_between(const uint32_t from, const uint32_t to)
{
  uint32_t i = __llair_choice();
  if (!(from <= i && i <= to))
    __llair_unreachable();
  return i;
}

#define PROD 0
#define CONS 1

#define CAPACITY 5

typedef _Atomic(uint8_t) atomic_uint8_t;
typedef _Atomic(uint32_t) atomic_uint32_t;

typedef struct queue_t {
  uint32_t dat[CAPACITY];
  atomic_uint8_t own[CAPACITY];
  atomic_uint32_t itr[CAPACITY];
  uint32_t cap;
  atomic_uint32_t pc;
  atomic_uint32_t cc;
} queue_t;

uint32_t
num_bytes_to_allocate()
{
  return sizeof(queue_t);
}

queue_t*
queue_init(void* const mem)
{
  uint32_t cap = CAPACITY;
  assert(cap >= 0);
  queue_t* const result = mem;
  for (uint32_t idx = 0; idx < cap; ++idx) {
    result->dat[idx] = 0;
    atomic_store(&result->own[idx], PROD);
    atomic_store(&result->itr[idx], 0);
  }
  result->cap = cap;
  atomic_store(&result->pc, cap);
  atomic_store(&result->cc, cap);
  return result;
}

bool
queue_is_empty(queue_t* const q)
{
  return atomic_load(&q->pc) == atomic_load(&q->cc);
}

uint32_t
start_enqueue(queue_t* const q)
{
  while (true) {
    cct_point();
    const uint32_t pc = atomic_load(&q->pc);
    const uint32_t i = pc / q->cap;
    const uint32_t k = pc % q->cap;
    cct_point();
    const uint32_t ik = atomic_load(&q->itr[k]);
    cct_point();
    const uint8_t ok = atomic_load(&q->own[k]);
    if (ik == i - 1 && ok == PROD) {
      uint32_t tmp_pc = pc;
      cct_point();
      if (atomic_compare_exchange_weak(&q->pc, &tmp_pc, pc + 1)) {
        return k;
      }
    }
  }
}

void
mark_ready(queue_t* const q, const uint32_t k)
{
  cct_point();
  atomic_store(&q->own[k], CONS);
  cct_point();
  atomic_fetch_add(&q->itr[k], 1);
}

uint32_t
start_dequeue(queue_t* const q)
{
  while (true) {
    cct_point();
    const uint32_t cc = atomic_load(&q->cc);
    const uint32_t i = cc / q->cap;
    const uint32_t k = cc % q->cap;
    cct_point();
    const uint8_t ok = atomic_load(&q->own[k]);
    cct_point();
    const uint32_t ik = atomic_load(&q->itr[k]);
    if (ik == i && ok == CONS) {
      uint32_t cc_tmp = cc;
      cct_point();
      if (atomic_compare_exchange_weak(&q->cc, &cc_tmp, cc + 1)) {
        return k;
      }
    }
  }
}

void
mark_free(queue_t* const q, const uint32_t k)
{
  cct_point();
  atomic_store(&q->own[k], PROD);
}

queue_t* test_queue;

static void
produce_thread_run()
{
  void* const arg = test_queue;
  if (arg == NULL) {
    return;
  }
  queue_t* const q = (queue_t*)arg;
  const uint32_t d = cct_random_between(1, 100);
  const uint32_t idx = start_enqueue(q);
  q->dat[idx] = d;
  mark_ready(q, idx);
  return;
}

static void
consume_thread_run()
{
  void* const arg = test_queue;
  if (arg == NULL) {
    return;
  }
  queue_t* const q = (queue_t*)arg;
  const uint32_t idx = start_dequeue(q);
  const uint32_t d = q->dat[idx];
  mark_free(q, idx);
  return;
}

#define NUM_PRODUCE_THREADS 2
#define NUM_CONSUME_THREADS 2
static_assert(NUM_PRODUCE_THREADS >= NUM_CONSUME_THREADS,
    "#produce threads >= #consume threads");

/* First runs all produce threads to finish, then runs all consume threads to
 * finish.
 */
int
main(void)
{
  void* test_mem_ptr = __llair_alloc(num_bytes_to_allocate());
  test_queue = queue_init(test_mem_ptr);
  thread_t* produce_threads[NUM_PRODUCE_THREADS];
  thread_t* consume_threads[NUM_CONSUME_THREADS];
  error_t status;

  for (uint32_t i = 0; i < NUM_PRODUCE_THREADS; i++) {
    status = thread_create(&produce_threads[i], &produce_thread_run);
    assert(OK == status && "Failed to create thread");
  }
  for (uint32_t i = 0; i < NUM_PRODUCE_THREADS; i++) {
    status = thread_join(produce_threads[i]);
    assert(OK == status && "Failed to join thread");
  }

  for (uint32_t i = 0; i < NUM_CONSUME_THREADS; i++) {
    status = thread_create(&consume_threads[i], &consume_thread_run);
    assert(OK == status && "Failed to create thread");
  }
  for (uint32_t i = 0; i < NUM_CONSUME_THREADS; i++) {
    status = thread_join(consume_threads[i]);
    assert(OK == status && "Failed to join thread");
  }

  if (NUM_PRODUCE_THREADS == NUM_CONSUME_THREADS) {
    assert(queue_is_empty(test_queue) && "Non-empty queue");
  }

  free(test_mem_ptr);

  return 0;
}
