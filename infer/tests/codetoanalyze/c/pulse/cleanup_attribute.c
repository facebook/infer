/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>
#include <string.h>

void cleanup_char(char** x) { free(*x); }

void cleanup_int(int** x) { free(*x); }

void no_cleanup(int** x) { /* nothing */ }

void cleanup_malloc_ok() {
  __attribute__((cleanup(cleanup_int))) int* x;
  x = malloc(sizeof(int));
  if (x != NULL) {
    *x = 10;
  }
  /* x goes out of scope. Cleanup function called - no leak */
}

void FN_wrong_cleanup_malloc_bad() {
  // no_cleanup does nothing, hence there is a leak, but Pulse treats values
  // stored in variables with a cleanup function as always reachable
  __attribute__((cleanup(no_cleanup))) int* x;
  x = malloc(sizeof(int));
}

// related to https://github.com/facebook/infer/issues/8
void cleanup_string_ok() {
  __attribute__((cleanup(cleanup_char))) char* s;
  s = strdup("demo string");
  /* s goes out of scope. Cleanup function called - no leak */
}

void cleanup_double_free_bad() {
  __attribute__((cleanup(cleanup_int))) int* x = malloc(sizeof(int));
  free(x);
}

void cleanup_set_to_null_ok() {
  __attribute__((cleanup(cleanup_int))) int* x = malloc(sizeof(int));
  free(x);
  x = NULL;
}

void deref_cleanup(int** x) { **x = 42; }

void cleanup_null_deref_bad() {
  __attribute__((cleanup(deref_cleanup))) int* x = NULL;
}

int* cleanup_return_freed_value() {
  __attribute__((cleanup(cleanup_int))) int* x = malloc(sizeof(int));
  return x;
}

void call_cleanup_return_freed_value_bad() {
  int* x = cleanup_return_freed_value();
  if (x != NULL) {
    *x = 42;
  }
}

int* take_ptr(int** x) {
  int* p = *x;
  *x = NULL;
  return p;
}

int* cleanup_return_taken_value() {
  __attribute__((cleanup(cleanup_int))) int* x = malloc(sizeof(int));
  // the return value is computed before the cleanup function runs
  return take_ptr(&x);
}

void call_cleanup_return_taken_value_ok() {
  int* x = cleanup_return_taken_value();
  if (x != NULL) {
    *x = 42;
  }
  free(x);
}

void cleanup_in_loop_ok(int n) {
  for (int i = 0; i < n; i++) {
    __attribute__((cleanup(cleanup_int))) int* x = malloc(sizeof(int));
    if (i == 0) {
      continue;
    }
    if (i == 1) {
      break;
    }
  }
}

void cleanup_double_free_on_break_bad(int n) {
  for (int i = 0; i < n; i++) {
    __attribute__((cleanup(cleanup_int))) int* x = malloc(sizeof(int));
    if (i == 1) {
      free(x);
      break;
    }
  }
}

void cleanup_in_statement_expression_bad() {
  int r = ({
    __attribute__((cleanup(cleanup_int))) int* x = malloc(sizeof(int));
    free(x);
    0;
  });
}

int cleanup_in_returned_statement_expression_bad() {
  return ({
    __attribute__((cleanup(cleanup_int))) int* x = malloc(sizeof(int));
    free(x);
    0;
  });
}

void read_cleanup(int*** y) { int v = ***y; }

void cleanup_reverse_order_ok() {
  __attribute__((cleanup(cleanup_int))) int* x = malloc(sizeof(int));
  if (x == NULL) {
    return;
  }
  *x = 42;
  // y is cleaned up before x
  __attribute__((cleanup(read_cleanup))) int** y = &x;
}

void FN_cleanup_goto_out_of_scope_bad(int b) {
  {
    __attribute__((cleanup(cleanup_int))) int* x = malloc(sizeof(int));
    if (b) {
      free(x);
      // the cleanup function is not called when goto leaves the scope
      goto out;
    }
  }
out:
  return;
}
