/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <limits.h>
#include <stddef.h>

struct queried_state {
  int count;
};

int query_state(const struct queried_state* s);

int query_offset_then_write(struct queried_state* s) {
  int before = query_state(s);
  s->count = 0;
  return before + 1;
}

// Forgetting the call equality must preserve arithmetic about its old result.
// The caller also needs the callee's entry-state query to equal its own query.
void related_results_after_write_ok(struct queried_state* s) {
  int before = query_state(s);
  int after = query_offset_then_write(s);
  if (after != before + 1) {
    int* p = NULL;
    *p = 42;
  }
}

void related_results_after_write_bad(struct queried_state* s) {
  int before = query_state(s);
  int after = query_offset_then_write(s);
  if (after == before + 1) {
    int* p = NULL;
    *p = 42;
  }
}

int bounded_query_after_write(struct queried_state* s, int bound) {
  // Keep an entry-state query, then forget a different result after mutation.
  query_state(s);
  s->count = 0;
  int value = query_state(s);
  s->count = 1;
  if (value > bound) {
    return 1;
  }
  return 0;
}

// Projection must preserve the finite range of the forgotten result in callers.
void bounded_query_after_write_ok(struct queried_state* s) {
  if (bounded_query_after_write(s, INT_MAX)) {
    int* p = NULL;
    *p = 42;
  }
}

void bounded_query_after_write_bad(struct queried_state* s) {
  if (bounded_query_after_write(s, INT_MIN)) {
    int* p = NULL;
    *p = 42;
  }
}

int global_queue_size;
int global_queue_count(void);
void global_queue_pop(void);

// FN: an unknown zero-argument call can read global memory, but has no actual
// from which forget_pure_calls_reading can discover that dependency.
void FN_drain_global_queue_bad(void) {
  if (global_queue_count() == 0) {
    return;
  }
  while (global_queue_count() != 0) {
    global_queue_pop();
  }
  int* p = NULL;
  *p = 42;
}

// FN: direct global writes do not invalidate zero-argument queries either.
void FN_global_count_after_reset_bad(void) {
  int before = global_queue_count();
  global_queue_size = 0;
  if (global_queue_count() != before) {
    int* p = NULL;
    *p = 42;
  }
}

void reset_global_queue(void) { global_queue_size = 0; }

// FN: the same missing dependency occurs when the write is in a callee.
void FN_unknown_read_after_callee_global_write_bad(void) {
  int before = global_queue_count();
  reset_global_queue();
  if (global_queue_count() != before) {
    int* p = NULL;
    *p = 42;
  }
}
