/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

namespace array_init {

struct S {
  int x;
};

int empty_braces_bad() {
  S* a[4] = {};
  return a[3]->x;
}

int nullptr_filler_bad() {
  S* a[4] = {nullptr};
  return a[2]->x;
}

struct Table {
  S* entries[4];
};

int struct_array_member_filler_bad() {
  Table t = {{nullptr}};
  return t.entries[2]->x;
}

int nested_array_filler_bad() {
  S* m[2][3] = {{nullptr}};
  return m[1][2]->x;
}

struct Pair {
  S* p;
  int y;
};

int aggregate_filler_bad() {
  Pair a[3] = {{nullptr, 1}};
  return a[2].p->x;
}

struct WithDefault {
  S* p = nullptr;
  int y = 5;
};

int default_member_initializer_filler_bad() {
  WithDefault a[3] = {};
  return a[1].p->x;
}

struct WithConstructor {
  WithConstructor() : p(nullptr) {}
  S* p;
};

int constructor_filler_bad() {
  WithConstructor a[3] = {};
  return a[2].p->x;
}

struct WithVirtual {
  virtual void f() {}
  S* p;
};

int zero_initialized_constructor_filler_bad() {
  WithVirtual a[2] = {};
  return a[1].p->x;
}

int new_array_filler_bad() {
  S** a = new S*[4]{nullptr};
  int r = a[3]->x;
  delete[] a;
  return r;
}

struct MemberInit {
  S* p[4];
  MemberInit() : p{} {}
};

int member_initializer_filler_bad() {
  MemberInit m;
  return m.p[3]->x;
}

int explicit_before_filler_ok() {
  S s{1};
  S* a[4] = {&s};
  return a[0]->x;
}

// the remaining elements exceed --clang-compound-literal-init-limit, so the
// array is zero-initialized with a builtin that is not modelled
int FN_large_filler_bad() {
  S* a[16] = {nullptr};
  return a[10]->x;
}

struct Owner {
  Owner() : p((int*)malloc(sizeof(int))) {}
  ~Owner() { free(p); }
  int* p;
};

// the destructors of array elements are not called
void FP_constructor_filler_leak_ok() { Owner a[2] = {}; }

} // namespace array_init
