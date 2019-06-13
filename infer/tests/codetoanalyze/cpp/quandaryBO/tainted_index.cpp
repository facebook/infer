/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

extern int __infer_taint_source();
extern void __infer_taint_sink(int i);

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

struct arg {
  int taint;
  int bo;
};

arg multi_level_source_ok() {
  return {.taint = __infer_taint_source(), .bo = 12};
}

void multi_level_sink_ok(int taint, int bo) {
  __infer_taint_sink(taint);
  int arr[10];
  arr[bo] = 0;
}

void multi_level_bad() {
  int i = multi_level_source_bad();
  multi_level_sink_bad(i);
}

void multi_level_good() { int i = multi_level_source_bad(); }

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
