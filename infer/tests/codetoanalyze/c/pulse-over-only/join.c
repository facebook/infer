/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// best run with --pulse-max-disjuncts 0 --pulse-over-approximate-reasoning

#include <stdlib.h>

int join_two_valid_values() {
  int x = 42;
  if (random()) {
    x = 0;
  }
  return x;
}

int join_two_valid_pointers(int* p) {
  *p = 42;
  if (random()) {
    *p = 0;
  }
  return *p;
}

int join_valid_invalid_pointers(int* p) {
  *p = 42;
  if (random()) {
    free(p);
  }
  return *p;
}

int join_two_invalid_pointers(int* p) {
  if (random()) {
    free(p);
  } else {
    free(p);
  }
  return *p;
}

struct node {
  int data;
  struct node* next;
};

int join_with_heap_cycle(struct node* n) {
  if (random()) {
    n = n->next;
  }
  return n->data;
}

int join_with_heap_cycle2(struct node* n) {
  n->data = 42;
  if (random()) {
    n = n->next;
  } else {
    n->next = NULL;
  }
  return n->data;
}

//       /-- n2 --\
// n1 --<          >-- n4
//       \-- n3 --/
int join_fork_then_meet_pointers(struct node* n1,
                                 struct node* n2,
                                 struct node* n3,
                                 struct node* n4) {
  // force all pointers to be allocated
  n1->data = 1;
  n2->data = 2;
  n3->data = 3;
  // set the meet point
  n2->next = n4;
  n3->next = n4;
  // set the fork point
  if (random()) {
    n1->next = n2;
  } else {
    n1->next = n3;
  }
  return n1->next->next->data; // n4->data
}

void a_few_join_cases1(int x, int y, int* p, int* q) {
  if (y > 0) {
    *p = 42;
  } else {
    *p = 0;
  }
  *q = 0;

  if (x == y) {
    q = p;
  }

  if (x == 444) {
    *q = y;
  }
}

void a_few_join_cases2(int x, int y, int* p, int* q) {
  if (random()) {
    *p = 42;
  } else {
    *p = 0;
  }
  // *p \in { 0, 42 }

  if (random()) {
    *q = 777;
  } else {
    q = NULL;
  }
  // q = NULL \/ q|->777

  if (x == 444) {
    *p = y;
  }
  // *p \in {0, 42, y}
}

void continue_after_fatal_bad2() {
  int x;
  int* p;
  if (random()) {
    p = NULL;
  } else {
    p = &x;
  }
  *p = 42;
  int* q = NULL;
  *q = 52;
}
