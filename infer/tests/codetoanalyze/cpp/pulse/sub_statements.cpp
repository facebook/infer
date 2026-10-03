/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <vector>

namespace sub_statements {

struct Vec {
  int size = 0;
  // binding a literal argument to the reference materializes a temporary
  void push_back(const int& x) { size++; }
};

void if_temporary_bad() {
  Vec v;
  int c = 1;
  if (c)
    v.push_back(42);
  if (v.size == 1) {
    int* p = nullptr;
    *p = 42;
  }
}

void else_temporary_bad() {
  Vec v;
  int c = 0;
  if (c) {
  } else
    v.push_back(42);
  if (v.size == 1) {
    int* p = nullptr;
    *p = 42;
  }
}

void label_temporary_bad() {
  Vec v;
  goto out;
out:
  v.push_back(42);
  if (v.size == 1) {
    int* p = nullptr;
    *p = 42;
  }
}

void case_temporary_bad() {
  Vec v;
  int k = 1;
  switch (k) {
    case 1:
      v.push_back(42);
  }
  if (v.size == 1) {
    int* p = nullptr;
    *p = 42;
  }
}

void default_temporary_bad() {
  Vec v;
  int k = 2;
  switch (k) {
    case 1:
      break;
    default:
      v.push_back(42);
  }
  if (v.size == 1) {
    int* p = nullptr;
    *p = 42;
  }
}

void do_while_temporary_bad() {
  Vec v;
  do
    v.push_back(42);
  while (0);
  if (v.size == 1) {
    int* p = nullptr;
    *p = 42;
  }
}

void for_temporary_bad() {
  Vec v;
  for (int i = 0; i < 1; i++)
    v.push_back(42);
  if (v.size == 1) {
    int* p = nullptr;
    *p = 42;
  }
}

void while_temporary_bad() {
  Vec v;
  int i = 0;
  while (i++ < 1)
    v.push_back(42);
  if (v.size == 1) {
    int* p = nullptr;
    *p = 42;
  }
}

static void push_if(Vec& v, int c) {
  if (c)
    v.push_back(42);
}

void call_push_if_bad() {
  Vec v;
  push_if(v, 1);
  if (v.size == 1) {
    int* p = nullptr;
    *p = 42;
  }
}

struct Arr {
  int arr[2];
  std::vector<int> vec;
};

// Pulse drops the paths of a loop that needs 10 iterations, so no path reaches
// the end of the scope of `sorted`, where Pulse checks whether it was modified.
// The early return reaches the exit, so the copy is reported anyway.
void FP_copy_modified_before_long_loop_ok(Vec& v, Arr& a, int c) {
  if (!c) {
    return;
  }
  if (c)
    v.push_back(42);
  Arr sorted = a;
  sorted.arr[0] = 42;
  for (int i = 0; i != 10; i++) {
  }
}

// a statement that only names a variable, e.g. `c;`, has no instructions and no
// CFG node

void if_nodeless_bad() {
  int c = 1;
  if (c)
    c;
  int* p = nullptr;
  *p = 42;
}

void else_nodeless_bad() {
  int c = 0;
  if (c) {
  } else
    c;
  int* p = nullptr;
  *p = 42;
}

void label_nodeless_bad() {
  int c = 0;
  goto out;
out:
  c;
  int* p = nullptr;
  *p = 42;
}

void case_nodeless_bad() {
  int k = 1;
  switch (k) {
    case 1:
      k;
  }
  int* p = nullptr;
  *p = 42;
}

void default_nodeless_bad() {
  int k = 2;
  switch (k) {
    case 1:
      break;
    default:
      k;
  }
  int* p = nullptr;
  *p = 42;
}

void do_while_nodeless_bad() {
  int c = 0;
  do
    c;
  while (0);
  int* p = nullptr;
  *p = 42;
}

void for_nodeless_bad() {
  for (int i = 0; i < 1; i++)
    i;
  int* p = nullptr;
  *p = 42;
}

void while_nodeless_bad() {
  int i = 0;
  while (i++ < 1)
    i;
  int* p = nullptr;
  *p = 42;
}

} // namespace sub_statements
