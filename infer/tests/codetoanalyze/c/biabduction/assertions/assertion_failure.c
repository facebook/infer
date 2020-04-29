/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>
#include <stdlib.h>

void simple_check(int x) { assert(x < 3); }

void simple_assertion_failure() {
  int x = 4;
  simple_check(x);
}

void no_assertion_failure() {
  int x = 2;
  simple_check(x);
}

typedef struct {
  int value;
} node;

void check_node(node* n) { assert(n->value < 3); }

node* assertion_failure_with_heap() {
  node* n = malloc(sizeof(node));
  if (n != NULL) {
    n->value = 4;
    check_node(n);
  }
  return n;
}

node* no_assertion_failure_with_heap() {
  node* n = malloc(sizeof(node));
  if (n != NULL) {
    n->value = 2;
    check_node(n);
  }
  return n;
}

void __infer_fail(char*);

void my_assert(int x) {
  if (!x) {
    __infer_fail("ASSERTION_FAILURE");
  }
}

void should_not_report_assertion_failure(int x) { my_assert(x); }

void should_report_assertion_failure(int x) {
  x = 0;
  my_assert(x);
}

int global;

void check_global() { assert(global != 0); }

void skip() {}

void assignment_after_check() {
  check_global();
  global = 0;
  skip();
}

void assignemt_before_check() {
  global = 0;
  check_global();
}

void failure_on_both_branches(int x) {
  if (x > 3) {
    simple_check(x);
  } else {
    simple_check(42);
  }
}
