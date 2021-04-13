/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifndef TREIBER_STACK
#define TREIBER_STACK

#include <stdatomic.h>
#include <stdbool.h>

/**
 * Treiber, R.K.: Systems programming: Coping with parallelism. Technical Report
 * RJ 5118, IBM Almaden Research Center (1986)
 */

typedef int data_type;
#define EMPTY -1

typedef struct node_t {
  data_type d;
  struct node_t* n;
} node;

typedef struct stack_t {
  _Atomic(node*) top;
} stack;

void init_stack(stack* s);
void push(stack* s, data_type v);
data_type pop(stack* s);

int unsafe_size(stack* s);
void print(stack* s);

#endif
