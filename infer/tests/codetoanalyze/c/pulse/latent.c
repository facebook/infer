/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void conditional_free2(int b, int* x) {
  if (b) {
    free(x);
  }
}

void latent_use_after_free(int b, int* x) {
  conditional_free2(b, x);
  *x = 42;
  if (!b) {
    // just to avoid memory leaks
    free(x);
  }
}

void manifest_use_after_free(int* x) { latent_use_after_free(1, x); }

void deref_then_free_then_deref_bad(int* x) {
  *x = 42;
  free(x);
  *x = 42;
}

void create_branching(int b) {
  if (b) {
  }
}

void nonlatent_use_after_free_bad(int b, int* x) {
  // the branch is independent of the issue here, so we should report the issue
  // in this function
  create_branching(b);
  free(x);
  *x = 42;
}

// same as above but branch after freeing
void nonlatent_use_after_free_bad2(int b, int* x) {
  free(x);
  create_branching(b);
  *x = 42;
}

// all latent issues that reach main are manifest, so this should be called
// "main_bad" but that would defeat the actual point :)
int main(int argc, char** argv) {
  int* x = malloc(sizeof(int));
  if (x) {
    latent_use_after_free(argc, x);
  }
}

// *not* latent because callers have no way to influence &x inside of the
// function
void equal_to_stack_address_test_then_crash_bad(int x, int* y) {
  if (y == &x) {
    int* p = NULL;
    *p = 42;
  }
}

void crash_if_different_addresses(int* x, int* y) {
  *x = 42;
  *y = 52;
  if (x != y) {
    int* p = NULL;
    *p = 42;
  }
}

struct node {
  int data;
  struct node* next;
};

void traverse_and_crash_if_equal_to_root(struct node* p) {
  struct node* old_p = p;
  while (p != NULL) {
    p = p->next;
    if (old_p == p) {
      int* crash = NULL;
      *crash = 42;
    }
  }
}

void crash_after_one_node_bad(struct node* q) {
  q->next = q;
  traverse_and_crash_if_equal_to_root(q);
}

void crash_after_two_nodes_bad(struct node* q) {
  q->next->next = q;
  traverse_and_crash_if_equal_to_root(q);
}

void FN_crash_after_six_nodes_bad(struct node* q) {
  q->next->next->next->next->next->next = q;
  traverse_and_crash_if_equal_to_root(q);
}

int global_flag;
int global_flag2;

void branch_on_global() {
  if (global_flag) {
  }
}

// the branch on the global in the callee is independent of the issue
void null_deref_after_branch_on_global_bad() {
  branch_on_global();
  int* p = NULL;
  *p = 42;
}

void use_after_free_after_branch_on_global_bad(int* x) {
  branch_on_global();
  free(x);
  *x = 42;
}

void nested_branches_on_globals() {
  if (global_flag) {
    if (global_flag2) {
    }
  }
}

void null_deref_after_nested_branches_on_globals_bad() {
  nested_branches_on_globals();
  int* p = NULL;
  *p = 42;
}

void null_deref_after_two_branches_on_global_bad() {
  branch_on_global();
  branch_on_global();
  int* p = NULL;
  *p = 42;
}

void branch_on_global2() {
  if (global_flag2) {
  }
}

// both globals are 0 on one of the paths
void null_deref_after_branches_on_two_globals_bad() {
  branch_on_global();
  branch_on_global2();
  int* p = NULL;
  *p = 42;
}

void branch_three_ways_on_global() {
  if (global_flag < 0) {
  } else if (global_flag == 0) {
  }
}

void null_deref_after_three_way_branch_on_global_bad() {
  branch_three_ways_on_global();
  int* p = NULL;
  *p = 42;
}

struct flags {
  int enabled;
};

struct flags global_flags;

void branch_on_global_field() {
  if (global_flags.enabled) {
  }
}

void null_deref_after_branch_on_global_field_bad() {
  branch_on_global_field();
  int* p = NULL;
  *p = 42;
}

void branch_on_field(struct flags* f) {
  if (f->enabled) {
  }
}

void null_deref_after_branch_on_field_bad(struct flags* f) {
  branch_on_field(f);
  int* p = NULL;
  *p = 42;
}

// the parameter and the global are both 0 on one of the paths
void null_deref_after_branch_on_global_and_test_bad(int a) {
  branch_on_global();
  if (a == 0) {
    int* p = NULL;
    *p = 42;
  }
}

void set_to_42(int* p) { *p = 42; }

// the null value is also the value of the global on one of the paths
void null_deref_in_callee_after_branch_on_global_bad() {
  branch_on_global();
  set_to_42(NULL);
}

void null_deref_after_range_check_and_branch_on_global_bad(int len) {
  if (len <= 0 || len > 100) {
    return;
  }
  branch_on_global();
  int* p = NULL;
  *p = 42;
}

int* null_if(int b, int* x) {
  if (b) {
    return NULL;
  }
  return x;
}

// latent: the issue happens only when the parameter is not 0
void null_deref_if_param_after_branch_on_global_latent(int b, int* x) {
  branch_on_global();
  int* p = null_if(b, x);
  *p = 42;
}

void null_deref_after_branch_on_global_in_callee_bad(int* x) {
  null_deref_if_param_after_branch_on_global_latent(1, x);
}

void free_if_global_flag(int* x) {
  if (global_flag) {
    free(x);
  }
}

// latent: the issue happens only when the global is set
void use_after_free_if_global_flag_latent(int* x) {
  free_if_global_flag(x);
  *x = 42;
}

void free_if_global_flag2(int* x) {
  if (global_flag2) {
    free(x);
  }
}

// latent: the issue happens only when one of the globals is set
void use_after_free_if_either_global_flag_latent(int* x) {
  free_if_global_flag(x);
  free_if_global_flag2(x);
  *x = 42;
}

void free_if_global_flag_is(int b, int* x) {
  if (b == 1) {
    if (global_flag) {
      free(x);
    }
  } else if (!global_flag) {
    free(x);
  }
}

// latent: whatever the value of the parameter, the issue happens only for some
// values of the global
void use_after_free_if_global_flag_is_latent(int a, int* x) {
  int b = 2;
  if (a > 0) {
    b = 1;
  }
  free_if_global_flag_is(b, x);
  *x = 42;
}

int* null_if_global_flag(int* x) {
  if (global_flag) {
    return NULL;
  }
  return x;
}

// latent: the issue happens only when the global is set
void null_deref_if_global_flag_latent(int* x) {
  int* p = null_if_global_flag(x);
  *p = 42;
}

int* null_unless_global_flag(int* x) {
  if (!global_flag) {
    return NULL;
  }
  return x;
}

// latent: the issue happens only when the global is 0, like the parameter
void null_deref_unless_global_flag_after_test_latent(int a, int* x) {
  if (a == 0) {
    int* p = null_unless_global_flag(x);
    *p = 42;
  }
}

// FN because the null dereference happens on a different line depending on
// the global, so each of them is latent
void FN_null_deref_whatever_global_flag_bad(int* x) {
  int* p = null_if_global_flag(x);
  int* q = null_unless_global_flag(x);
  *p = 42;
  *q = 42;
}

// not reported once per caller either
void call_null_deref_whatever_global_flag1(int* x) {
  FN_null_deref_whatever_global_flag_bad(x);
}

void call_null_deref_whatever_global_flag2(int* x) {
  FN_null_deref_whatever_global_flag_bad(x);
}

// the two parameters are equal on the path to the issue only because they are
// both 0, which is not an aliasing assumption
void two_params_equal_to_zero_bad(int a, int b) {
  if (a == 0 && b == 0) {
    int* p = NULL;
    *p = 42;
  }
}

int* null_if_equal_to_global_flag(int a, int* x) {
  if (a == global_flag) {
    return NULL;
  }
  return x;
}

// latent: the global is 0 like the parameter only because the callee assumes
// that they are equal
void null_deref_if_global_flag_equal_to_zero_latent(int a, int* x) {
  if (a == 0) {
    int* p = null_if_equal_to_global_flag(a, x);
    *p = 42;
  }
}

int* null_if_equal(int a, int b, int* x) {
  if (a == b) {
    return NULL;
  }
  return x;
}

// latent: same with two parameters
void null_deref_if_params_equal_to_zero_latent(int a, int b, int* x) {
  int* p = null_if_equal(a, b, x);
  if (a == 0) {
    *p = 42;
  }
}

void branch_on_aliasing(int* x, int* y) {
  if (x == y) {
  }
}

// FN because the precondition of one of the paths assumes that the parameters
// are aliases
void FN_null_deref_after_branch_on_aliasing_bad(int* x, int* y) {
  branch_on_aliasing(x, y);
  int* p = NULL;
  *p = 42;
}

int* global_ptr;

// suppressed because the null value of `p` comes from the assignment to the
// global, which is not on the path of the access
void null_deref_if_null_after_branch_latent(int b, int* p) {
  global_ptr = NULL;
  if (!p) {
    create_branching(b);
    *p = 42;
  }
}

void null_deref_after_branch_in_callee_bad() {
  null_deref_if_null_after_branch_latent(1, NULL);
}
