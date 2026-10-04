/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

void assign_implicit_cast_ok() {
  bool* b = (bool*)malloc(sizeof(bool));
  uint16_t i = 1;
  if (b) {
    *b = true;
    *b = !i;
    if (*b) {
      int* p = 0;
      *p = 5;
    }
    free(b);
  }
}

void assign_implicit_cast_bad() {
  bool* b = (bool*)malloc(sizeof(bool));
  uint16_t i = 0;
  if (b) {
    *b = false;
    *b = !i;
    if (*b) {
      int* p = 0;
      *p = 5;
    }
    free(b);
  }
}

void assign_paren_ok() {
  bool* b = (bool*)malloc(sizeof(bool));
  int x = 42, y = 33;
  if (b) {
    *b = true;
    *b = (x == y);
    if (*b) {
      int* p = 0;
      *p = 5;
    }
    free(b);
  }
}

void assign_paren_bad() {
  bool* b = (bool*)malloc(sizeof(bool));
  int x = 42, y = 42;
  if (b) {
    *b = false;
    *b = (x == y);
    if (*b) {
      int* p = 0;
      *p = 5;
    }
    free(b);
  }
}

void block_scope_function_declaration_bad() {
  int* defined_after_use_returns_null(void);
  int* p = defined_after_use_returns_null();
  *p = 42;
}

int* defined_after_use_returns_null(void) { return NULL; }

void block_scope_function_and_var_declarations_bad() {
  int unknown_function(void), x = 3;
  if (x == 3) {
    int* p = NULL;
    *p = 42;
  }
}

void block_scope_static_assert_bad() {
  _Static_assert(sizeof(int) >= 2, "int is too small");
  int* p = NULL;
  *p = 42;
}

void local_label_declaration_bad() {
  __label__ done;
  int* p = NULL;
  goto done;
done:
  *p = 42;
}

#define FREE_UNLESS(cond, p) \
  do {                       \
    __label__ skip;          \
    if (cond) {              \
      goto skip;             \
    }                        \
    free(p);                 \
  skip:;                     \
  } while (0)

void local_labels_with_same_name_ok(int c, int* p, int* q) {
  FREE_UNLESS(c, p);
  FREE_UNLESS(c, q);
}

void local_labels_with_same_name_bad(int c, int* p, int* q) {
  FREE_UNLESS(c, p);
  FREE_UNLESS(c, q);
  free(p);
}

// the address of the current instruction, as in the Linux kernel
#define THIS_IP              \
  ({                         \
    __label__ __here;        \
  __here:                    \
    (unsigned long)&&__here; \
  })

static void uses_this_ip(void) {
  unsigned long ip = THIS_IP;
  (void)ip;
}

void caller_of_this_ip_user_bad() {
  int* p = NULL;
  uses_this_ip();
  *p = 42;
}

void local_label_value_of_statement_expression_ok() {
  int v = ({
    __label__ here;
  here:
    5;
  });
  if (v != 5) {
    int* p = NULL;
    *p = 42;
  }
}

void local_label_effect_in_statement_expression_bad() {
  int* p = NULL;
  int v = ({
    __label__ here;
  here:
    *p;
  });
}
