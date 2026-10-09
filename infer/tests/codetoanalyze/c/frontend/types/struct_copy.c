/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct Point {
  int x;
  int y;
};

struct WithArrays {
  int small[2];
  struct Point points[2];
  int matrix[2][2];
  int large[10];
  int empty[10][0];
};

struct ManyArrayElements {
  struct Point points[5];
  int matrix[2][2];
  int small[2];
  int k;
};

void copy_init(struct WithArrays* p) { struct WithArrays s = *p; }

void assign(struct WithArrays* p, struct WithArrays* q) { *p = *q; }

void copy_many_array_elements(struct ManyArrayElements* p) {
  struct ManyArrayElements s = *p;
}
