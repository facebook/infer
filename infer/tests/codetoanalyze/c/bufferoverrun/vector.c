/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

typedef int int4 __attribute__((vector_size(16)));

void vector_init_then_overrun_Bad() {
  int4 v = {1, 2, 3, 4};
  int a[4];
  a[4] = v[0];
}

void vector_init_index_Good() {
  int4 v = {1, 2, 3, 4};
  int a[4];
  a[v[3] - 1] = 0;
}

// vector elements are not distinguished from each other: v[0] reads the last
// element stored
void FP_vector_init_index_Good() {
  int4 v = {1, 2, 3, 20};
  int a[10];
  a[v[0]] = 0;
}

// v[0] reads 4, so the condition is reported as always false
int FP_vector_init_condition_Good() {
  int4 v = {1, 2, 3, 4};
  if (v[0] == 1) {
    return 1;
  }
  return 0;
}
