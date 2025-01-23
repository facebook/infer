/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* note that the goal is flag the existence of recursive or mutually-recursive
   calls, not non-termination, so flagging a well-founded recursion is not
   considered a false positive */

void trivial_recursive_bad() { trivial_recursive_bad(); }

int factorial_bad(int x) {
  if (x > 0) {
    return x * factorial_bad(x - 1);
  } else {
    return 1;
  }
}

void mutual1_bad();

void mutual3_bad() { mutual1_bad(); }

void mutual2_bad() { mutual3_bad(); }

void mutual1_bad() { mutual2_bad(); }

int global;

// should report MUTUAL_RECURSION not INFINITE_RECURSION
void recursive_modify_global_ok(int x) {
  if (global > 0) {
    global--;
    recursive_modify_global_ok(x);
  }
}

void infinite_recursion_unchanged_global_bad(int x) {
  if (global > 0) {
    global = global;
    infinite_recursion_unchanged_global_bad(x);
  }
}

struct data {
  int a;
  int b;
};

void recursion_on_field_ok(struct data* x) {
  if (x->a > 0) {
    x->a = x->a - 1;
    recursion_on_field_ok(x);
  }
}

void set_fields(struct data* x, int a, int b) {
  x->b = b;
  x->a = a;
}

void recursion_on_fields_bad(struct data* x) {
  // materialize fields of x in the order "a, b" in the pre
  int a = x->a;
  int b = x->b;
  // trick pulse into writing the fields of x in the order "b, a" in the post
  set_fields(x, a, b);
  recursion_on_fields_bad(x);
}

#include "recursion2.h"

void across_file_1() { across_file_2(); }
