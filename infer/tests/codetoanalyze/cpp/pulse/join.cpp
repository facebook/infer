/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct foo {
  int* val;
};

struct list {
  struct list* next;
  struct foo* foo;
};

int visit_list(struct list* head, int cond) {
  int* result = 0;
  struct list* x = head;
  if (cond) {
    result = x->next->foo->val;
    delete result;
  } else {
    x = x->next;
    struct list* y = x->next;
    result = x->foo->val;
  }
  return *result;
}
