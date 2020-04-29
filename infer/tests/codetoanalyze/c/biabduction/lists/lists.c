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

/* Divide by zero error shows that we get a spec for add2 */
int lists_main() {
  int res = add2(0);
  return 5 / res;
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

int call_delete() {
  T* root = malloc(sizeof(T));
  delete_one(root); // no memory leak should be reported here
  return 0;
}

int call_delete_all1() {
  T* root = malloc(sizeof(T));
  delete_all(root); // no memory leak should be reported here
  return 0;
}

int call_delete_all2() {
  T* root = malloc(sizeof(T));
  if (root != NULL) {
    root->next = malloc(sizeof(T));
  }
  delete_all(root); // no memory leak should be reported here
  return 0;
}

int call_all() {
  int ret1 = call_delete();
  int ret2 = call_delete_all1();
  int ret3 = call_delete_all2();
  // if any of the callees have precondition not met, we won't see div by zero
  // here
  return 1 / (ret1 + ret2 + ret3);
}
