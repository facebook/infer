/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

typedef struct {
  int *val;
} box;

void box_set_both(box *p, box *q) {
  *p->val = 1;
  *q->val = 2;
}

void box_call_if_aliased(box *p, box *q) {
  if (p->val == q->val) box_set_both(p, q);
}

void box_always_call(box *p, box *q) {
  box_set_both(p, q);
}

void box_call_depending_on_alias(box *p, box *q) {
  box_call_if_aliased(p, q);
  box_always_call(p, q);
}

void box_test_aliased() {
  int x = 0;
  box a = {&x};
  box b = {&x};
  box_call_depending_on_alias(&a, &b);
}

void box_test_not_aliased() {
  int x = 0;
  int y = 1;
  box a = {&x};
  box b = {&y};
  box_call_depending_on_alias(&a, &b);
}
