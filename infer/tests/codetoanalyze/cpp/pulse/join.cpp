/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <vector>

void basic_loop_count_ok(int n) {
  for (int i = 0; i < n; i++) {
  }
}

struct foo {
  int* val;
};

struct list {
  struct list* next;
  struct foo* foo;
};

int invalidate_node_alias_bad(struct list* head, int cond) {
  int* result = 0;
  struct list* x = head;
  if (cond) {
    result = x->next->foo->val;
    delete result;
  } else {
    x = x->next;
    struct list* y = x->next;
    result = x->foo->val;
    delete result;
  }
  return *result;
}

void list_delete_ok(struct list** l) {
  auto head = *l;
  *l = nullptr;
  while (head) {
    auto tmp = head;
    head = head->next;
    if (tmp->foo) {
      free(tmp->foo);
      tmp->foo = nullptr;
    }
  }
}

struct BasicStruct {
  void some_method() {}
  BasicStruct();
  ~BasicStruct();
};

int nested_loops_ok() {
  while (true) {
    BasicStruct x;
    for (;;) {
      x.some_method();
    }
  }
}

extern bool some_bool();
extern BasicStruct mk_basic_struct();

void cond_inside_loop_ok() {
  while (true) {
    BasicStruct x;
    if (some_bool()) {
      x = mk_basic_struct();
    }

    x.some_method();
  }
}

void nested_loops3_ok(std::vector<BasicStruct>* c) {
  for (auto& b : *c) {
    (&b)->~BasicStruct();
  }
}
