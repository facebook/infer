/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
// modifies x (a mutable reference argument)
void modify_mut_ref_impure(int* x) { ++*x; }

int primitive_pure(int x) { x++; }

int fact_pure(int n) {
  int f = 1;
  while (n > 0)
    f *= n--;
  return f;
}

struct node {
  struct node* next;
  int data;
};
void create_cycle_impure(struct node* x) { x->next = x; }

void invalidate_local_impure(int** pp) {
  int t = 0xdeadbeef;
  *pp = &t; // <-- potential bug here since t goes out of scope
}
