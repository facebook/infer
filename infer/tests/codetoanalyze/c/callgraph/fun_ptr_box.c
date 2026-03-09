/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef int (*fun_ptr2)(int, int);

typedef struct {
  fun_ptr2 val;
} box2;

int box2_add(int x, int y) { return x + y; }

int box2_mul(int x, int y) { return x * y; }

int box2_invoke0(box2 *g, int x) { return g->val(x, 0); }

int box2_invoke1(box2 *g, int x) { return g->val(x, 1); }

int box2_apply(int (*f)(box2 *, int), box2 *g, int a) { return f(g, a); }

int box2_test_add_0() {
  box2 g = {box2_add};
  return box2_apply(box2_invoke0, &g, 3);
}

int box2_test_add_1() {
  box2 g = {box2_add};
  return box2_apply(box2_invoke1, &g, 3);
}

int box2_test_mul_0() {
  box2 g = {box2_mul};
  return box2_apply(box2_invoke0, &g, 3);
}

int box2_test_mul_1() {
  box2 g = {box2_mul};
  return box2_apply(box2_invoke1, &g, 3);
}
