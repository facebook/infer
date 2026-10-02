/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef int int4 __attribute__((vector_size(16)));
typedef float float4 __attribute__((ext_vector_type(4)));

struct vec_and_int {
  int4 v;
  int n;
};

struct vec_array {
  int4 val[2];
};

int vector_init() {
  int4 v = {1, 2, 3, 4};
  return v[0];
}

int vector_partial_init() {
  int4 v = {1};
  return v[0];
}

int vector_empty_init() {
  int4 v = {};
  return v[0];
}

int vector_copy_init(int4 w) {
  int4 v = {w};
  return v[0];
}

float ext_vector_init(float x) {
  float4 v = {x, 2, 3.0f, 4};
  return v[0];
}

int vector_compound_literal() { return ((int4){1, 2, 3, 4})[1]; }

int vector_in_struct_init() {
  struct vec_and_int s = {{1, 2, 3, 4}, 5};
  return s.v[0];
}

int vector_array_init() {
  struct vec_array s = {{{1, 2, 3, 4}, {5, 6, 7, 8}}};
  return s.val[1][0];
}

int vector_init_side_effect(int* p) {
  int4 v = {(*p)++, 2, 3, 4};
  return v[0];
}

double complex_init() {
  _Complex double z = {1.0, 2.0};
  return __real__ z;
}

double complex_init_single() {
  _Complex double z = {1.0};
  return __real__ z;
}

int bitint_init() {
  _BitInt(37) x = {5};
  return x;
}
