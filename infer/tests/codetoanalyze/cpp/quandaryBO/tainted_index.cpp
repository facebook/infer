/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

extern int __infer_taint_source();

void basic_bad() {
  int arr[10];
  int source = __infer_taint_source();
  arr[source] = 2;
}

int multi_level_source_bad() { return __infer_taint_source(); }

void multi_level_sink_bad(int i) {
  int arr[10];
  if (i > 0)
    arr[i] = 2;
}

void multi_level_bad() {
  int i = multi_level_source_bad();
  multi_level_sink_bad(i);
}

void memory_alloc_bad1_FN() { int arr[__infer_taint_source()]; }

void memory_alloc_bad2() {
  int s = __infer_taint_source();
  if (s <= 2147483647) {
    int arr[s];
  }
}

struct st {
  int size;
  int ind;
};

st overlapping_issues_source_good() {
  return {.size = __infer_taint_source(), .ind = 10};
}

void overlapping_issues_sink_good(st info) {
  int arr[info.size];
  arr[info.ind] = 0;
}

void overlapping_issues_good() {
  overlapping_issues_sink_good(overlapping_issues_source_good());
}
