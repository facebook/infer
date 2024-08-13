/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

struct l2 {
  int b;
  struct l2* a;
};

int add2(struct l2* l) {
  int r = 0;
  for (; l; l = l->a) {
    r += l->b;
  }
  return r;
}

int call_add2_then_deref_null_bad() {
  int res = add2(NULL);
  if (res == 0) {
    int* p = NULL;
    *p = 42; // reachable
  }
}

typedef struct node {
  struct node* next;
} T;

void delete_one(T* x) { free(x); }

void delete_all(T* x) {
  T* temp;
  while (x != NULL) {
    temp = x;
    x = x->next;
    free(temp);
  }
}

int call_delete_one_ok() {
  T* root = malloc(sizeof(T));
  delete_one(root); // no memory leak should be reported here
  return 0;
}

int call_delete_all1_ok() {
  T* root = malloc(sizeof(T));
  delete_all(root); // no memory leak should be reported here
  return 0;
}

int call_delete_all2_ok() {
  T* root = malloc(sizeof(T));
  if (root != NULL) {
    root->next = malloc(sizeof(T));
  }
  delete_all(root); // no memory leak should be reported here
  return 0;
}
