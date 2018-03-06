/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

void realloc_Good_FP() {
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

void realloc_struct1_Bad_FN() {
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

void realloc_struct2_Bad_FN() {
  struct S3* buf1 = (struct S3*)malloc(2 * sizeof(struct S3));
  buf1[0].s.x = 5;
  struct S3* buf2 = (struct S3*)realloc(buf1, 5 * sizeof(struct S3));
  buf2[buf2[0].s.x].s.x = 0;
}
