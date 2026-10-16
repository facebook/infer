/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

struct S {
  int x;
};

struct WithArray {
  struct S* ptrs[2];
};

struct Outer {
  int k;
  struct WithArray inner;
};

struct WithStructArray {
  struct WithArray elts[2];
};

struct WithLargeArray {
  struct S* ptrs[6];
};

struct WithLarge2DArray {
  struct S* ptrs[2][3];
};

struct WithManyArrayElements {
  struct WithArray elts[5];
  int counts[5];
  int flags[2];
};

int copy_init_array_field_bad() {
  struct WithArray a = {{NULL, NULL}};
  struct WithArray b = a;
  return b.ptrs[0]->x;
}

int assign_array_field_bad() {
  struct WithArray a = {{NULL, NULL}};
  struct WithArray b;
  b = a;
  return b.ptrs[1]->x;
}

int assign_overwrites_array_field_ok(struct S* s) {
  struct WithArray a = {{s, s}};
  struct WithArray b = {{NULL, NULL}};
  b = a;
  return b.ptrs[0]->x;
}

int copy_is_independent_ok(struct S* s) {
  struct WithArray a = {{s, s}};
  struct WithArray b = a;
  b.ptrs[0] = NULL;
  return a.ptrs[0]->x;
}

int copy_nested_array_field_bad() {
  struct Outer o = {0, {{NULL, NULL}}};
  struct Outer o2 = o;
  return o2.inner.ptrs[1]->x;
}

int copy_array_of_structs_bad() {
  struct WithStructArray a = {{{{NULL, NULL}}, {{NULL, NULL}}}};
  struct WithStructArray b = a;
  return b.elts[1].ptrs[0]->x;
}

struct WithArray make_null_array() {
  struct WithArray a = {{NULL, NULL}};
  return a;
}

int return_array_field_bad() {
  struct WithArray b = make_null_array();
  return b.ptrs[0]->x;
}

int conditional_copy_array_field_bad(int c, struct S* s) {
  struct WithArray a = {{NULL, NULL}};
  struct WithArray b = {{s, NULL}};
  struct WithArray d = c ? a : b;
  return d.ptrs[0]->x;
}

// arrays longer than --clang-compound-literal-init-limit are copied as a whole,
// which does not copy their elements
int FN_copy_large_array_field_bad() {
  struct WithLargeArray a = {{NULL}};
  struct WithLargeArray b = a;
  return b.ptrs[0]->x;
}

// the limit applies to the number of elements of multi-dimensional arrays
int FN_copy_large_2d_array_field_bad() {
  struct WithLarge2DArray a = {{{NULL}}};
  struct WithLarge2DArray b = a;
  return b.ptrs[0][0]->x;
}

// arrays longer than the limit are copied as a whole, so b keeps its old
// elements
int FP_assign_large_array_field_ok(struct S* s) {
  struct WithLargeArray a = {{s, s, s, s, s, s}};
  struct WithLargeArray b = {{NULL}};
  b = a;
  return b.ptrs[0]->x;
}

// arrays are copied as a whole in copies of more than 16 values
int FN_copy_many_array_elements_bad() {
  struct WithManyArrayElements a = {{{{NULL, NULL}}}};
  struct WithManyArrayElements b = a;
  return b.elts[0].ptrs[0]->x;
}
