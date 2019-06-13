/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <cstdlib>

void realloc_Good() {
  int* buf1 = (int*)malloc(2 * sizeof(int));
  buf1[0] = 3;
  int* buf2 = (int*)realloc(buf1, 5 * sizeof(int));
  buf2[buf2[0]] = 0;
}

void realloc_Bad() {
  int* buf1 = (int*)malloc(2 * sizeof(int));
  buf1[0] = 5;
  int* buf2 = (int*)realloc(buf1, 5 * sizeof(int));
  buf2[buf2[0]] = 0;
}

void realloc2_Good() {
  int* buf1 = (int*)malloc(2 * sizeof(int));
  for (int i = 0; i < 2; i++) {
    buf1[i] = 3;
  }
  int* buf2 = (int*)realloc(buf1, 5 * sizeof(int));
  buf2[buf2[0]] = 0;
}

struct S1 {
  int x[2];
};

void realloc_struct1_Good() {
  struct S1* buf1 = (struct S1*)malloc(2 * sizeof(struct S1));
  buf1[0].x[0] = 3;
  struct S1* buf2 = (struct S1*)realloc(buf1, 5 * sizeof(struct S1));
  buf2[buf2[0].x[0]].x[0] = 0;
}

void realloc_struct1_Bad() {
  struct S1* buf1 = (struct S1*)malloc(2 * sizeof(struct S1));
  buf1[0].x[0] = 5;
  struct S1* buf2 = (struct S1*)realloc(buf1, 5 * sizeof(struct S1));
  buf2[buf2[0].x[0]].x[0] = 0;
}

struct S2 {
  int x;
};

struct S3 {
  struct S2 s;
};

void realloc_struct2_Good() {
  struct S3* buf1 = (struct S3*)malloc(2 * sizeof(struct S3));
  buf1[0].s.x = 3;
  struct S3* buf2 = (struct S3*)realloc(buf1, 5 * sizeof(struct S3));
  buf2[buf2[0].s.x].s.x = 0;
}

void realloc_struct2_Bad() {
  struct S3* buf1 = (struct S3*)malloc(2 * sizeof(struct S3));
  buf1[0].s.x = 5;
  struct S3* buf2 = (struct S3*)realloc(buf1, 5 * sizeof(struct S3));
  buf2[buf2[0].s.x].s.x = 0;
}

struct S4 {
  int a[3];
  int c[3];
  int b[1];
};

struct S5 {
 public:
  int d[3];
  int f[3];
  S4 e;
};

void realloc_flexible_array_Good() {
  struct S5* buf1 = (struct S5*)malloc(sizeof(struct S5) + 4 * sizeof(int));
  struct S5* buf2 =
      (struct S5*)realloc(buf1, sizeof(struct S5) + 9 * sizeof(int));
  buf2->e.b[7] = 0;
}

void realloc_flexible_array_Bad() {
  struct S5* buf1 = (struct S5*)malloc(sizeof(struct S5) + 9 * sizeof(int));
  struct S5* buf2 =
      (struct S5*)realloc(buf1, sizeof(struct S5) + 4 * sizeof(int));
  buf2->e.b[7] = 0;
}
