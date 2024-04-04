/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void latent_use_after_free(int b, int* x) {
  if (b) {
    free(x);
  }
  *x = 42;
  if (!b) {
    // just to avoid memory leaks
    free(x);
  }
}

void manifest_use_after_free(int* x) { latent_use_after_free(1, x); }

void deref_then_free_then_deref_bad(int* x) {
  *x = 42;
  free(x);
  *x = 42;
}

// FN because it's flagged only as latent at the moment
void FN_nonlatent_use_after_free_bad(int b, int* x) {
  // the branch is independent of the issue here, so we should report the issue
  // in this function
  if (b) {
  }
  free(x);
  *x = 42;
}

// all latent issues that reach main are manifest, so this should be called
// "main_bad" but that would defeat the actual point :)
int main(int argc, char** argv) {
  int* x = malloc(sizeof(int));
  if (x) {
    latent_use_after_free(argc, x);
  }
}

// *not* latent because callers have no way to influence &x inside of the
// function
void equal_to_stack_address_test_then_crash_bad(int x, int* y) {
  if (y == &x) {
    int* p = NULL;
    *p = 42;
  }
}

void crash_if_different_addresses(int* x, int* y) {
  *x = 42;
  *y = 52;
  if (x != y) {
    int* p = NULL;
    *p = 42;
  }
}

struct node {
  int data;
  struct node *next;
};

void traverse_and_crash_if_equal_to_root(struct node *p) {
  struct node* old_p = p;
  while(p != NULL) {
    p = p->next;
    if (old_p == p) {
      int* crash = NULL;
      *crash = 42;
    }
  }
}

void crash_after_one_node_bad(struct node *q) {
  q->next = q;
  traverse_and_crash_if_equal_to_root(q);
}

void crash_after_two_nodes_bad(struct node *q) {
  q->next->next = q;
  traverse_and_crash_if_equal_to_root(q);
}

void FN_crash_after_six_nodes_bad(struct node *q) {
  q->next->next->next->next->next->next = q;
  traverse_and_crash_if_equal_to_root(q);
}
