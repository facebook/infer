/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "treiber_stack.h"
#include <stdio.h>
#include <stdlib.h>

void init_stack(stack* s) { s->top = NULL; }

void push(stack* s, data_type v) {
  /*L0:*/ node* new_node = malloc(sizeof(node));
  node* top_snapshot = NULL;
  new_node->d = v;
  do {
    /*L1:*/ top_snapshot = atomic_load(&s->top);
    new_node->n = top_snapshot;
  }
  /*L2:*/
  while (!atomic_compare_exchange_weak(&s->top, &top_snapshot, new_node));
}

data_type pop(stack* s) {
  node* top_snapshot = NULL;
  node* tn = NULL;
  do {
    /*L0:*/ top_snapshot = s->top;
    if (top_snapshot == NULL) {
      return EMPTY;
    }
    /*L1:*/ tn = top_snapshot->n;
  }
  /*L2:*/
  while (!atomic_compare_exchange_weak(&s->top, &top_snapshot, tn));
  data_type r = top_snapshot->d;
  return r;
}

int unsafe_size(stack* s) {
  int result = 0;
  node* curr = s->top;
  while (curr != NULL) {
    ++result;
    curr = curr->n;
  }
  return result;
}

void print(stack* s) {
  node* curr = s->top;
  printf("[");
  while (curr != NULL) {
    printf("%d", curr->d);
    if (curr->n != NULL) {
      printf(" ");
    }
    curr = curr->n;
  }
  printf("]");
}
