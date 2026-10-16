/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <string.h>

struct node {
  int data;
  struct node* next;
};

void log_message(const char* msg);

// the null branch falls through to the dereference: reported as
// COMPARED_TO_NULL_AND_DEREFERENCED here and as NULLPTR_DEREFERENCE in callers
// that pass null
int get_data_compared_to_null_bad(struct node* n) {
  if (n == NULL) {
    log_message("null node");
  }
  return n->data;
}

int call_get_data_compared_to_null_with_null_bad() {
  return get_data_compared_to_null_bad(NULL);
}

int call_get_data_compared_to_null_ok() {
  struct node n = {42, NULL};
  return get_data_compared_to_null_bad(&n);
}

int wrap_get_data_compared_to_null(struct node* n) {
  return get_data_compared_to_null_bad(n);
}

int call_wrap_get_data_compared_to_null_with_null_bad() {
  return wrap_get_data_compared_to_null(NULL);
}

int check_then_get_data_compared_to_null_bad(struct node* n) {
  if (n == NULL) {
    log_message("null node");
  }
  return get_data_compared_to_null_bad(n);
}

int call_check_then_get_data_compared_to_null_with_null_bad() {
  return check_then_get_data_compared_to_null_bad(NULL);
}

int get_data_compared_to_null_else_bad(struct node* n) {
  if (n != NULL) {
    log_message("non-null node");
  } else {
    log_message("null node");
  }
  return n->data;
}

int call_get_data_compared_to_null_else_with_null_bad() {
  return get_data_compared_to_null_else_bad(NULL);
}

int get_next_data_compared_to_null_bad(struct node* n) {
  struct node* next = n->next;
  if (next == NULL) {
    log_message("no next node");
  }
  return next->data;
}

int call_get_next_data_compared_to_null_with_null_bad() {
  struct node n = {42, NULL};
  return get_next_data_compared_to_null_bad(&n);
}

size_t length_compared_to_null_bad(const char* s) {
  if (s == NULL) {
    log_message("null string");
  }
  return strlen(s);
}

size_t call_length_compared_to_null_with_null_bad() {
  return length_compared_to_null_bad(NULL);
}

int is_zero(int x) { return x == 0; }

// the error is latent here because it depends on the result of [is_zero]
int get_data_compared_to_null_if_zero_bad(struct node* n, int x) {
  if (is_zero(x)) {
    if (n == NULL) {
      log_message("null node");
    }
    return n->data;
  }
  return 0;
}

int call_get_data_compared_to_null_if_zero_with_null_bad() {
  return get_data_compared_to_null_if_zero_bad(NULL, 0);
}

int call_get_data_compared_to_null_if_zero_with_null_ok() {
  return get_data_compared_to_null_if_zero_bad(NULL, 1);
}

// [!n] does not count as a comparison to null so only callers are reported
int get_data_negated_check(struct node* n) {
  if (!n) {
    log_message("null node");
  }
  return n->data;
}

int call_get_data_negated_check_with_null_bad() {
  return get_data_negated_check(NULL);
}

int get_data_compared_to_null_local_bad(struct node* n) {
  struct node* empty = NULL;
  if (n == empty) {
    log_message("null node");
  }
  return n->data;
}

int call_get_data_compared_to_null_local_with_null_bad() {
  return get_data_compared_to_null_local_bad(NULL);
}

int last_data;

// Pulse identifies [n] with the constant 0 written in the null branch, which
// makes [n] itself an invalid address
int get_data_negated_check_reset_bad(struct node* n) {
  if (!n) {
    last_data = 0;
  }
  return n->data;
}

int call_get_data_negated_check_reset_with_null_bad() {
  return get_data_negated_check_reset_bad(NULL);
}

int get_data_or_default_ok(struct node* n) {
  if (n == NULL) {
    return -1;
  }
  return n->data;
}

int call_get_data_or_default_with_null_ok() {
  return get_data_or_default_ok(NULL);
}

int get_data_or_abort_ok(struct node* n) {
  if (n == NULL) {
    abort();
  }
  return n->data;
}

int call_get_data_or_abort_with_null_ok() { return get_data_or_abort_ok(NULL); }

void fatal_error(const char* msg);

// [fatal_error] does not return but Infer cannot know it without a definition
// or a noreturn attribute, so the null branch reaches the dereference (same
// with [!n])
int FP_get_data_compared_to_null_or_fatal_ok(struct node* n) {
  if (n == NULL) {
    fatal_error("null node");
  }
  return n->data;
}

int FP_call_get_data_compared_to_null_or_fatal_with_null_ok() {
  return FP_get_data_compared_to_null_or_fatal_ok(NULL);
}

struct node* same_node(struct node* n) { return n; }

int get_data_of_same_node_bad(struct node* n) {
  struct node* m = same_node(n);
  if (n == NULL) {
    log_message("null node");
  }
  return m->data;
}

int call_get_data_of_same_node_with_null_bad() {
  return get_data_of_same_node_bad(NULL);
}

// on the path where [n] is null, [m] is null too and Pulse identifies them,
// but the null dereference does not depend on the caller: it is only reported
// here
int get_data_of_null_local_bad(struct node* n) {
  struct node* m = NULL;
  if (n == NULL) {
    log_message("null node");
  }
  return m->data;
}

int call_get_data_of_null_local_with_null_ok() {
  return get_data_of_null_local_bad(NULL);
}

struct node* lookup(struct node* key);

// the result of an unknown call does not come from the caller, even when the
// caller passes null to the call
int get_data_of_lookup_bad(struct node* n) {
  if (n == NULL) {
    log_message("null node");
  }
  struct node* m = lookup(n);
  if (m == NULL) {
    log_message("not found");
  }
  return m->data;
}

int call_get_data_of_lookup_with_null_ok() {
  return get_data_of_lookup_bad(NULL);
}

struct node* lookup_wrapper(struct node* key) { return lookup(key); }

int get_data_of_lookup_wrapper_bad(struct node* n) {
  if (n == NULL) {
    log_message("null node");
  }
  struct node* m = lookup_wrapper(n);
  if (m == NULL) {
    log_message("not found");
  }
  return m->data;
}

int call_get_data_of_lookup_wrapper_with_null_ok() {
  return get_data_of_lookup_wrapper_bad(NULL);
}

char* get_buffer(size_t size);

int fill_buffer_bad(size_t len) {
  if (len == 0) {
    log_message("empty buffer");
  }
  char* buf = get_buffer(len + 1);
  if (buf == NULL) {
    log_message("out of memory");
  }
  buf[0] = 'a';
  return 0;
}

int call_fill_buffer_with_zero_ok() { return fill_buffer_bad(0); }
